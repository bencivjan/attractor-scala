package ai.attractor.pipeline.validation

import ai.attractor.pipeline.parser.{Graph, Node, Edge}
import ai.attractor.pipeline.condition.ConditionEvaluator

// ---------------------------------------------------------------------------
// Diagnostics
// ---------------------------------------------------------------------------

enum Severity:
  case Error, Warning, Info

case class Diagnostic(
    rule: String,
    severity: Severity,
    message: String,
    nodeId: Option[String] = None,
    edge: Option[(String, String)] = None,
    fix: Option[String] = None
)

// ---------------------------------------------------------------------------
// Lint rule trait
// ---------------------------------------------------------------------------

trait LintRule:
  def name: String
  def apply(graph: Graph): List[Diagnostic]

// ---------------------------------------------------------------------------
// Built-in lint rules
// ---------------------------------------------------------------------------

private object StartNodeRule extends LintRule:
  val name = "start_node"
  def apply(graph: Graph): List[Diagnostic] =
    val starts = graph.nodes.values.filter(_.shape == "Mdiamond").toList
    starts.size match
      case 1 => Nil
      case 0 =>
        List(Diagnostic(
          name, Severity.Error,
          "Graph must have exactly one start node (shape=Mdiamond); none found",
          fix = Some("Add a node with shape=Mdiamond as the entry point")
        ))
      case n =>
        List(Diagnostic(
          name, Severity.Error,
          s"Graph must have exactly one start node (shape=Mdiamond); found $n: ${starts.map(_.id).mkString(", ")}",
          fix = Some("Remove extra start nodes so only one remains")
        ))

private object TerminalNodeRule extends LintRule:
  val name = "terminal_node"
  def apply(graph: Graph): List[Diagnostic] =
    val terminals = graph.nodes.values.filter(_.shape == "Msquare").toList
    if terminals.nonEmpty then Nil
    else
      List(Diagnostic(
        name, Severity.Error,
        "Graph must have at least one terminal node (shape=Msquare); none found",
        fix = Some("Add a node with shape=Msquare as an exit point")
      ))

private object ReachabilityRule extends LintRule:
  val name = "reachability"
  def apply(graph: Graph): List[Diagnostic] =
    val starts = graph.nodes.values.filter(_.shape == "Mdiamond").toList
    starts match
      case start :: _ =>
        val reachable = bfs(start.id, graph)
        val unreachable = graph.nodes.keySet -- reachable
        unreachable.toList.sorted.map: nodeId =>
          Diagnostic(
            name, Severity.Error,
            s"Node '$nodeId' is not reachable from start node '${start.id}'",
            nodeId = Some(nodeId),
            fix = Some(s"Add an edge path from '${start.id}' to '$nodeId'")
          )
      // Cannot check reachability without a start node; StartNodeRule flags this.
      case Nil => Nil

  private def bfs(startId: String, graph: Graph): Set[String] =
    val visited = scala.collection.mutable.Set[String](startId)
    val queue = scala.collection.mutable.Queue[String](startId)
    while queue.nonEmpty do
      val current = queue.dequeue()
      for edge <- graph.outgoingEdges(current) do
        if !visited.contains(edge.to) then
          visited += edge.to
          queue.enqueue(edge.to)
    visited.toSet

private object EdgeTargetExistsRule extends LintRule:
  val name = "edge_target_exists"
  def apply(graph: Graph): List[Diagnostic] =
    graph.edges.flatMap: edge =>
      val problems = List.newBuilder[Diagnostic]
      if !graph.nodes.contains(edge.from) then
        problems += Diagnostic(
          name, Severity.Error,
          s"Edge source '${edge.from}' does not reference an existing node",
          edge = Some((edge.from, edge.to)),
          fix = Some(s"Define node '${edge.from}' or correct the edge source")
        )
      if !graph.nodes.contains(edge.to) then
        problems += Diagnostic(
          name, Severity.Error,
          s"Edge target '${edge.to}' does not reference an existing node",
          edge = Some((edge.from, edge.to)),
          fix = Some(s"Define node '${edge.to}' or correct the edge target")
        )
      problems.result()

private object StartNoIncomingRule extends LintRule:
  val name = "start_no_incoming"
  def apply(graph: Graph): List[Diagnostic] =
    graph.nodes.values.filter(_.shape == "Mdiamond").flatMap: start =>
      val incoming = graph.incomingEdges(start.id)
      if incoming.isEmpty then Nil
      else
        List(Diagnostic(
          name, Severity.Error,
          s"Start node '${start.id}' must not have incoming edges; found ${incoming.size}",
          nodeId = Some(start.id),
          fix = Some(s"Remove incoming edges to start node '${start.id}'")
        ))
    .toList

private object ExitNoOutgoingRule extends LintRule:
  val name = "exit_no_outgoing"
  def apply(graph: Graph): List[Diagnostic] =
    graph.nodes.values.filter(_.shape == "Msquare").flatMap: exit =>
      val outgoing = graph.outgoingEdges(exit.id)
      if outgoing.isEmpty then Nil
      else
        List(Diagnostic(
          name, Severity.Error,
          s"Exit node '${exit.id}' must not have outgoing edges; found ${outgoing.size}",
          nodeId = Some(exit.id),
          fix = Some(s"Remove outgoing edges from exit node '${exit.id}'")
        ))
    .toList

private object ConditionSyntaxRule extends LintRule:
  val name = "condition_syntax"
  def apply(graph: Graph): List[Diagnostic] =
    graph.edges.flatMap: edge =>
      val cond = edge.condition
      if cond.isEmpty then Nil
      else
        ConditionEvaluator.validateSyntax(cond) match
          case Right(_) => Nil
          case Left(err) =>
            List(Diagnostic(
              name, Severity.Error,
              s"Edge ${edge.from} -> ${edge.to}: invalid condition syntax: $err",
              edge = Some((edge.from, edge.to)),
              fix = Some("Fix the condition expression syntax")
            ))

private object TypeKnownRule extends LintRule:
  val name = "type_known"

  private val knownTypes = Set(
    "", "codergen", "interviewer", "gateway", "tool",
    "human", "passthrough", "transform", "start", "end",
    "merge", "router", "validator"
  )

  def apply(graph: Graph): List[Diagnostic] =
    graph.nodes.values.flatMap: node =>
      val t = node.nodeType
      if knownTypes.contains(t) then Nil
      else
        List(Diagnostic(
          name, Severity.Warning,
          s"Node '${node.id}' has unrecognized type '$t'",
          nodeId = Some(node.id),
          fix = Some(s"Use a recognized type: ${knownTypes.filter(_.nonEmpty).toList.sorted.mkString(", ")}")
        ))
    .toList

private object FidelityValidRule extends LintRule:
  val name = "fidelity_valid"

  private val validFidelities = Set("low", "medium", "high", "max", "")

  def apply(graph: Graph): List[Diagnostic] =
    val nodeDiags = graph.nodes.values.flatMap: node =>
      node.fidelity match
        case Some(f) if !validFidelities.contains(f) =>
          List(Diagnostic(
            name, Severity.Warning,
            s"Node '${node.id}' has invalid fidelity '$f'",
            nodeId = Some(node.id),
            fix = Some(s"Use a valid fidelity: ${validFidelities.filter(_.nonEmpty).toList.sorted.mkString(", ")}")
          ))
        case _ => Nil

    val edgeDiags = graph.edges.flatMap: edge =>
      edge.fidelity match
        case Some(f) if !validFidelities.contains(f) =>
          List(Diagnostic(
            name, Severity.Warning,
            s"Edge ${edge.from} -> ${edge.to} has invalid fidelity '$f'",
            edge = Some((edge.from, edge.to)),
            fix = Some(s"Use a valid fidelity: ${validFidelities.filter(_.nonEmpty).toList.sorted.mkString(", ")}")
          ))
        case _ => Nil

    nodeDiags.toList ++ edgeDiags

private object RetryTargetExistsRule extends LintRule:
  val name = "retry_target_exists"
  def apply(graph: Graph): List[Diagnostic] =
    val nodeIds = graph.nodes.keySet
    val nodeDiags = graph.nodes.values.flatMap: node =>
      val primary = node.retryTarget.flatMap: target =>
        if nodeIds.contains(target) then None
        else Some(Diagnostic(
          name, Severity.Warning,
          s"Node '${node.id}' retry_target '$target' does not reference an existing node",
          nodeId = Some(node.id),
          fix = Some("Correct retry_target to reference an existing node")
        ))
      val fallback = node.fallbackRetryTarget.flatMap: target =>
        if nodeIds.contains(target) then None
        else Some(Diagnostic(
          name, Severity.Warning,
          s"Node '${node.id}' fallback_retry_target '$target' does not reference an existing node",
          nodeId = Some(node.id),
          fix = Some("Correct fallback_retry_target to reference an existing node")
        ))
      primary.toList ++ fallback.toList

    // Also check graph-level retry targets
    val graphPrimary = graph.retryTarget.flatMap: target =>
      if nodeIds.contains(target) then None
      else Some(Diagnostic(
        name, Severity.Warning,
        s"Graph retry_target '$target' does not reference an existing node",
        fix = Some("Correct graph-level retry_target to reference an existing node")
      ))
    val graphFallback = graph.fallbackRetryTarget.flatMap: target =>
      if nodeIds.contains(target) then None
      else Some(Diagnostic(
        name, Severity.Warning,
        s"Graph fallback_retry_target '$target' does not reference an existing node",
        fix = Some("Correct graph-level fallback_retry_target to reference an existing node")
      ))

    nodeDiags.toList ++ graphPrimary.toList ++ graphFallback.toList

private object GoalGateHasRetryRule extends LintRule:
  val name = "goal_gate_has_retry"
  def apply(graph: Graph): List[Diagnostic] =
    graph.nodes.values.flatMap: node =>
      if node.goalGate && node.retryTarget.isEmpty then
        List(Diagnostic(
          name, Severity.Warning,
          s"Node '${node.id}' has goal_gate=true but no retry_target",
          nodeId = Some(node.id),
          fix = Some("Add a retry_target attribute to the goal_gate node")
        ))
      else Nil
    .toList

private object PromptOnLlmNodesRule extends LintRule:
  val name = "prompt_on_llm_nodes"
  def apply(graph: Graph): List[Diagnostic] =
    graph.nodes.values.flatMap: node =>
      if node.nodeType == "codergen" && node.prompt.isEmpty && node.label == node.id then
        List(Diagnostic(
          name, Severity.Warning,
          s"Node '${node.id}' is type codergen but has no prompt or descriptive label",
          nodeId = Some(node.id),
          fix = Some("Add a prompt or label attribute to provide instructions for the LLM")
        ))
      else Nil
    .toList

// ---------------------------------------------------------------------------
// Validator
// ---------------------------------------------------------------------------

object Validator:

  private val builtInRules: List[LintRule] = List(
    StartNodeRule,
    TerminalNodeRule,
    ReachabilityRule,
    EdgeTargetExistsRule,
    StartNoIncomingRule,
    ExitNoOutgoingRule,
    ConditionSyntaxRule,
    TypeKnownRule,
    FidelityValidRule,
    RetryTargetExistsRule,
    GoalGateHasRetryRule,
    PromptOnLlmNodesRule
  )

  /** Run all lint rules and return all diagnostics. */
  def validate(graph: Graph, extraRules: List[LintRule] = Nil): List[Diagnostic] =
    (builtInRules ++ extraRules).flatMap(_.apply(graph))

  /**
   * Validate and partition results: if any Error-severity diagnostic exists,
   * return Left with all diagnostics; otherwise return Right with warnings/info.
   */
  def validateOrRaise(
      graph: Graph,
      extraRules: List[LintRule] = Nil
  ): Either[List[Diagnostic], List[Diagnostic]] =
    val all = validate(graph, extraRules)
    val hasErrors = all.exists(_.severity == Severity.Error)
    if hasErrors then Left(all)
    else Right(all)
