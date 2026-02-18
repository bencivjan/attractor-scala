package ai.attractor.pipeline.engine

import cats.effect.IO
import cats.syntax.all.*

import java.nio.file.{Files, Path, Paths}
import java.time.Instant
import java.util.UUID

import ai.attractor.pipeline.parser.{Edge, Graph, Node}
import ai.attractor.pipeline.state.{Checkpoint, Context, Outcome, StageStatus}
import ai.attractor.pipeline.condition.ConditionEvaluator
import ai.attractor.pipeline.handler.HandlerRegistry
import ai.attractor.pipeline.interviewer.{AutoApproveInterviewer, Interviewer}
import ai.attractor.pipeline.validation.{Validator, Severity}

import scala.concurrent.duration.*

// ---------------------------------------------------------------------------
// Pipeline configuration
// ---------------------------------------------------------------------------

/** Exponential backoff parameters for retried stages. */
case class BackoffConfig(
    initialDelayMs: Long = 1000L,
    backoffFactor: Double = 2.0,
    maxDelayMs: Long = 60000L,
    jitter: Boolean = true
)

/** Retry policy applied to handler execution. */
case class RetryPolicy(
    maxAttempts: Int = 3,
    backoff: BackoffConfig = BackoffConfig()
)

/** Top-level pipeline execution configuration. */
case class PipelineConfig(
    checkpointPath: Option[String] = None,
    maxSteps: Int = 1000,
    retryPolicy: RetryPolicy = RetryPolicy()
)

// ---------------------------------------------------------------------------
// Pipeline execution engine
// ---------------------------------------------------------------------------

/** The core pipeline execution engine. Traverses a parsed Graph from the
  * start node, executing handlers at each step, evaluating edge conditions
  * to select the next node, and enforcing goal gates at terminal nodes.
  *
  * The execution loop:
  *   1. Validate the graph
  *   2. Initialize context and run directory
  *   3. Traverse from start node:
  *      a. Execute handler with retry policy
  *      b. Record completion and update context
  *      c. Save checkpoint
  *      d. Select next edge via the 5-step algorithm
  *      e. Handle loop_restart
  *      f. Advance to next node
  *   4. Finalize: write final checkpoint and return outcome
  */
class PipelineEngine(
    handlerRegistry: HandlerRegistry,
    interviewer: Interviewer = AutoApproveInterviewer
):

  /** Run the pipeline to completion. */
  def run(
      graph: Graph,
      logsRoot: String,
      config: PipelineConfig = PipelineConfig()
  ): IO[Outcome] =
    for
      // Validate phase
      _ <- Validator.validateOrRaise(graph) match
        case Left(diags) =>
          val msg = diags.filter(_.severity == Severity.Error).map(d => s"  - ${d.message}").mkString("\n")
          IO.raiseError(new IllegalArgumentException(s"Pipeline validation failed:\n$msg"))
        case Right(_) => IO.unit

      // Initialize phase
      runId = UUID.randomUUID().toString.take(8)
      runDir = Paths.get(logsRoot, s"run-$runId")
      _ <- IO.blocking(Files.createDirectories(runDir))
      context <- initializeContext(graph)
      startNode <- findStartNode(graph)
      completedNodes = scala.collection.mutable.ListBuffer.empty[String]

      // Execute phase
      outcome <- executeLoop(
        graph = graph,
        currentNode = startNode,
        context = context,
        logsRoot = runDir.toString,
        config = config,
        completedNodes = completedNodes,
        runId = runId,
        step = 0
      )

      // Finalize phase
      snap <- context.snapshot
      _ <- saveCheckpoint(
        runDir,
        Checkpoint(Instant.now(), "terminal", completedNodes.toList, Map.empty, snap, Nil)
      )
    yield outcome

  /** Resume a pipeline from a checkpoint, continuing from a specific node
    * with a pre-populated context and list of already-completed nodes.
    */
  def resumeFrom(
      graph: Graph,
      logsRoot: String,
      startNode: Node,
      context: Context,
      completedNodes: List[String],
      config: PipelineConfig = PipelineConfig()
  ): IO[Outcome] =
    for
      // Validate phase
      _ <- Validator.validateOrRaise(graph) match
        case Left(diags) =>
          val msg = diags.filter(_.severity == Severity.Error).map(d => s"  - ${d.message}").mkString("\n")
          IO.raiseError(new IllegalArgumentException(s"Pipeline validation failed:\n$msg"))
        case Right(_) => IO.unit

      // Resume phase
      runId = UUID.randomUUID().toString.take(8)
      runDir = Paths.get(logsRoot, s"run-$runId")
      _ <- IO.blocking(Files.createDirectories(runDir))
      completed = scala.collection.mutable.ListBuffer.from(completedNodes)

      // Execute phase
      outcome <- executeLoop(
        graph = graph,
        currentNode = startNode,
        context = context,
        logsRoot = runDir.toString,
        config = config,
        completedNodes = completed,
        runId = runId,
        step = 0
      )

      // Finalize phase
      snap <- context.snapshot
      _ <- saveCheckpoint(
        runDir,
        Checkpoint(Instant.now(), "terminal", completed.toList, Map.empty, snap, Nil)
      )
    yield outcome

  // -------------------------------------------------------------------------
  // Core execution loop
  // -------------------------------------------------------------------------

  private def executeLoop(
      graph: Graph,
      currentNode: Node,
      context: Context,
      logsRoot: String,
      config: PipelineConfig,
      completedNodes: scala.collection.mutable.ListBuffer[String],
      runId: String,
      step: Int
  ): IO[Outcome] =
    if step >= config.maxSteps then
      IO.pure(Outcome.fail(failureReason = s"Pipeline exceeded maximum step count (${config.maxSteps})"))
    else
      // Check if terminal node
      val isTerminal = isTerminalNode(currentNode, graph)
      if isTerminal then
        checkGoalGates(graph, context).flatMap:
          case Some(retryTarget) =>
            // Goal gates unsatisfied -- jump to retry target
            graph.nodes.get(retryTarget) match
              case Some(targetNode) =>
                executeLoop(graph, targetNode, context, logsRoot, config, completedNodes, runId, step + 1)
              case None =>
                IO.pure(Outcome.fail(failureReason = s"Retry target '$retryTarget' not found in graph"))
          case None =>
            // All goal gates satisfied (or none defined)
            context.snapshot.map: snap =>
              Outcome.success(
                notes = s"Pipeline completed at terminal node '${currentNode.id}'",
                contextUpdates = snap
              )
      else
        for
          // Execute handler with retry
          outcome <- executeWithRetry(currentNode, context, graph, logsRoot, config.retryPolicy)

          // Record completion and update context
          _ = completedNodes += currentNode.id
          _ <- context.set(s"status.${currentNode.id}", outcome.status.toString.toLowerCase)
          _ <- context.applyUpdates(outcome.contextUpdates)

          // Save checkpoint
          snap <- context.snapshot
          _ <- saveCheckpoint(
            Paths.get(logsRoot),
            Checkpoint(Instant.now(), currentNode.id, completedNodes.toList, Map.empty, snap, Nil)
          )

          // Select next edge
          result <- selectEdge(currentNode, outcome, context, graph).flatMap:
            case None =>
              // No outgoing edge found -- treat as implicit terminal
              IO.pure(outcome)
            case Some(edge) =>
              graph.nodes.get(edge.to) match
                case Some(nextNode) =>
                  executeLoop(graph, nextNode, context, logsRoot, config, completedNodes, runId, step + 1)
                case None =>
                  IO.pure(Outcome.fail(failureReason = s"Next node '${edge.to}' not found in graph"))
        yield result

  // -------------------------------------------------------------------------
  // Handler execution with retry
  // -------------------------------------------------------------------------

  /** Execute a handler with exponential backoff retry on failure. */
  def executeWithRetry(
      node: Node,
      context: Context,
      graph: Graph,
      logsRoot: String,
      retryPolicy: RetryPolicy
  ): IO[Outcome] =
    val maxRetries = if node.maxRetries > 0 then node.maxRetries else retryPolicy.maxAttempts
    val handler = handlerRegistry.resolve(node)

    def loop(attempt: Int): IO[Outcome] =
      handler.execute(node, context, graph, logsRoot).flatMap: outcome =>
        if (outcome.status == StageStatus.Fail || outcome.status == StageStatus.Retry) && attempt < maxRetries then
          val delay = computeBackoffDelay(retryPolicy.backoff, attempt)
          IO.sleep(delay) *> loop(attempt + 1)
        else
          IO.pure(outcome)

    loop(0)

  private def computeBackoffDelay(config: BackoffConfig, attempt: Int): FiniteDuration =
    val rawMs = (config.initialDelayMs * math.pow(config.backoffFactor, attempt.toDouble)).toLong
    val clampedMs = math.min(rawMs, config.maxDelayMs)
    val finalMs =
      if config.jitter then (math.random() * clampedMs).toLong
      else clampedMs
    finalMs.millis

  // -------------------------------------------------------------------------
  // Edge selection -- 5-step algorithm
  // -------------------------------------------------------------------------

  /** Select the next edge from a node using the 5-step algorithm:
    *   1. Condition-matching edges (evaluate conditions against outcome + context)
    *   2. Preferred label match (normalized)
    *   3. Suggested next IDs from the outcome
    *   4. Highest weight
    *   5. Lexical tiebreak on target node id
    */
  def selectEdge(
      node: Node,
      outcome: Outcome,
      context: Context,
      graph: Graph
  ): IO[Option[Edge]] =
    val outEdges = graph.outgoingEdges(node.id)
    if outEdges.isEmpty then IO.pure(None)
    else
      // Step 1: Condition-matching edges (IO because condition evaluation needs context snapshot)
      outEdges
        .filter(_.condition.nonEmpty)
        .traverse: edge =>
          ConditionEvaluator.evaluateCondition(edge.condition, outcome, context).map(edge -> _)
        .map: evaluated =>
          val conditionEdges = evaluated.filter(_._2).map(_._1)
          if conditionEdges.nonEmpty then
            Some(bestByWeightThenLexical(conditionEdges))
          else
            // Step 2: Preferred label match
            val preferred = outcome.preferredLabel
            val labelMatch =
              if preferred.nonEmpty then
                val normalized = normalizeLabel(preferred)
                outEdges.find(e => normalizeLabel(e.label) == normalized)
              else None

            labelMatch.orElse:
              // Step 3: Suggested next IDs
              val suggestedEdges = outEdges.filter(e => outcome.suggestedNextIds.contains(e.to))
              if suggestedEdges.nonEmpty then Some(bestByWeightThenLexical(suggestedEdges))
              else
                // Step 4 & 5: Highest weight, then lexical tiebreak
                // Filter to only unconditional edges (condition is empty)
                val unconditional = outEdges.filter(_.condition.isEmpty)
                val candidates = if unconditional.nonEmpty then unconditional else outEdges
                Some(bestByWeightThenLexical(candidates))

  /** Pick the edge with highest weight, breaking ties by lexical order of target id. */
  def bestByWeightThenLexical(edges: List[Edge]): Edge =
    edges.sortBy(e => (-e.weight, e.to)).head

  /** Normalize a label for comparison: lowercase, trim whitespace, strip accelerator prefixes. */
  def normalizeLabel(label: String): String =
    val trimmed = label.trim.toLowerCase
    // Strip accelerator patterns: [K] ..., K) ..., K - ...
    val bracketPattern = """\[\w+\]\s*(.+)""".r
    val parenPattern   = """\w+\)\s*(.+)""".r
    val dashPattern    = """\w+\s*-\s*(.+)""".r
    trimmed match
      case bracketPattern(rest) => rest.trim
      case parenPattern(rest)   => rest.trim
      case dashPattern(rest)    => rest.trim
      case other                => other

  // -------------------------------------------------------------------------
  // Goal gate enforcement
  // -------------------------------------------------------------------------

  /** Check whether all goal_gate nodes have SUCCESS status.
    * Returns Some(retryTarget) if gates are unsatisfied, None if all pass.
    */
  private def checkGoalGates(graph: Graph, context: Context): IO[Option[String]] =
    val gateNodes = graph.nodes.values.filter(_.goalGate).toList
    if gateNodes.isEmpty then IO.pure(None)
    else
      gateNodes.traverse: node =>
        context.getString(s"status.${node.id}").map: statusStr =>
          node -> (statusStr == "success")
      .map: results =>
        val unsatisfied = results.filterNot(_._2).map(_._1)
        if unsatisfied.isEmpty then None
        else
          // Find retry target: node-level first, then graph-level, then fallback
          unsatisfied.headOption
            .flatMap(_.retryTarget)
            .orElse(graph.retryTarget)
            .orElse(graph.fallbackRetryTarget)

  // -------------------------------------------------------------------------
  // Initialization helpers
  // -------------------------------------------------------------------------

  private def isTerminalNode(node: Node, graph: Graph): Boolean =
    (node.shape == "Msquare" || node.nodeType == "exit") &&
      graph.outgoingEdges(node.id).isEmpty

  private def findStartNode(graph: Graph): IO[Node] =
    val startNodes = graph.nodes.values.filter: n =>
      n.shape == "Mdiamond" || n.nodeType == "start"
    startNodes.headOption match
      case Some(node) => IO.pure(node)
      case None =>
        IO.raiseError(new IllegalStateException("No start node found in graph"))

  /** Initialize context with graph-level attributes. */
  private def initializeContext(graph: Graph): IO[Context] =
    Context.of(graph.attributes.map((k, v) => k -> (v: Any)))

  private def saveCheckpoint(dir: Path, checkpoint: Checkpoint): IO[Unit] =
    IO.blocking(Files.createDirectories(dir)) *>
      checkpoint.save(dir.resolve("checkpoint.json").toString)
