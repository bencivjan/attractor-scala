package ai.attractor.pipeline.engine

import cats.effect.IO

import ai.attractor.pipeline.handler.*
import ai.attractor.pipeline.interviewer.{AutoApproveInterviewer, Interviewer}
import ai.attractor.pipeline.parser.{DotParser, Graph}
import ai.attractor.pipeline.state.{Checkpoint, Context, Outcome}
import ai.attractor.pipeline.transform.Transform

// ---------------------------------------------------------------------------
// High-level pipeline runner
// ---------------------------------------------------------------------------

/** Convenience entry point that ties together parsing, transforms, validation,
  * handler registration, and engine execution.
  *
  * Steps:
  *   1. Parse DOT source into a Graph (caller provides the Graph)
  *   2. Apply transforms (variable expansion, stylesheet, user-supplied)
  *   3. Validate the graph
  *   4. Build handler registry with all built-in handlers
  *   5. Create engine and run
  */
object PipelineRunner:

  /** Run a pipeline from a pre-parsed Graph. */
  def fromGraph(
      graph: Graph,
      logsRoot: String,
      backend: Option[CodergenBackend] = None,
      interviewer: Interviewer = AutoApproveInterviewer,
      transforms: List[Transform] = Nil,
      config: PipelineConfig = PipelineConfig()
  ): IO[Outcome] =
    // Apply transforms
    val transformedGraph = transforms.foldLeft(graph): (g, t) =>
      t.apply(g)

    // Build handler registry
    val registry = buildRegistry(backend, interviewer)

    // Create engine and run
    val engine = PipelineEngine(registry, interviewer)
    engine.run(transformedGraph, logsRoot, config)

  /** Run a pipeline from DOT source text.
    *
    * Requires a DOT parser to be available. Since the parser lives in
    * `ai.attractor.pipeline.parser` and may not yet be implemented,
    * this method accepts the DOT source and delegates to a parse step.
    * Callers should use `fromGraph` if they already have a parsed Graph.
    */
  def fromDotSource(
      dotSource: String,
      logsRoot: String,
      backend: Option[CodergenBackend] = None,
      interviewer: Interviewer = AutoApproveInterviewer,
      transforms: List[Transform] = Nil,
      config: PipelineConfig = PipelineConfig()
  ): IO[Outcome] =
    for
      graph <- DotParser.parse(dotSource) match
        case Right(g) => IO.pure(g)
        case Left(err) => IO.raiseError(new IllegalArgumentException(s"DOT parse error: $err"))
      outcome <- fromGraph(graph, logsRoot, backend, interviewer, transforms, config)
    yield outcome

  /** Resume a pipeline from a saved checkpoint.
    *
    * Loads the checkpoint, restores context, and continues execution
    * from the checkpoint's current node.
    */
  def resumeFromCheckpoint(
      graph: Graph,
      checkpointPath: String,
      logsRoot: String,
      backend: Option[CodergenBackend] = None,
      interviewer: Interviewer = AutoApproveInterviewer,
      transforms: List[Transform] = Nil,
      config: PipelineConfig = PipelineConfig()
  ): IO[Outcome] =
    for
      checkpoint <- Checkpoint.load(checkpointPath)
      // Apply transforms to graph
      transformedGraph = transforms.foldLeft(graph): (g, t) =>
        t.apply(g)
      // Restore context from checkpoint
      context <- Context.of(checkpoint.contextValues.map((k, v) => k -> (v: Any)))
      // Find the node to resume from
      resumeNode <- transformedGraph.nodes.get(checkpoint.currentNode) match
        case Some(node) => IO.pure(node)
        case None => IO.raiseError(new IllegalStateException(
          s"Checkpoint references node '${checkpoint.currentNode}' which is not in the graph"
        ))
      // Build handler registry and engine
      registry = buildRegistry(backend, interviewer)
      engine = PipelineEngine(registry, interviewer)
      // Resume from the checkpoint node with restored context
      outcome <- engine.resumeFrom(
        transformedGraph, logsRoot, resumeNode, context, checkpoint.completedNodes, config
      )
    yield outcome

  /** Build a HandlerRegistry with all built-in handlers registered. */
  private def buildRegistry(
      backend: Option[CodergenBackend],
      interviewer: Interviewer
  ): HandlerRegistry =
    val codergenHandler = CodergenHandler(backend)
    val waitHandler = WaitForHumanHandler(interviewer)

    val registry = HandlerRegistry(defaultHandler = codergenHandler)
    registry.register("start", StartHandler)
    registry.register("exit", ExitHandler)
    registry.register("codergen", codergenHandler)
    registry.register("wait.human", waitHandler)
    registry.register("conditional", ConditionalHandler)
    registry.register("parallel", ParallelHandler(registry))
    registry.register("parallel.fan_in", FanInHandler)
    registry.register("tool", ToolHandler)
    registry.register("stack.manager_loop", codergenHandler) // Manager loop uses codergen
    registry
