package ai.attractor.pipeline.handler

import cats.effect.IO
import cats.effect.syntax.all.*
import cats.syntax.all.*

import ai.attractor.pipeline.parser.{Graph, Node}
import ai.attractor.pipeline.state.{Context, Outcome, StageStatus}

// ---------------------------------------------------------------------------
// Parallel fan-out handler
// ---------------------------------------------------------------------------

/** Join policy for parallel branch aggregation. */
enum JoinPolicy:
  case WaitAll, FirstSuccess, KOfN, Quorum

/** Error policy for parallel branch failures. */
enum ErrorPolicy:
  case FailFast, Continue, Ignore

/** Executes fan-out edges concurrently and aggregates results according to
  * the node's `join_policy` and `error_policy` attributes.
  *
  * Node attributes:
  *   - `join_policy`: wait_all (default), first_success, k_of_n, quorum
  *   - `error_policy`: fail_fast (default), continue, ignore
  *   - `max_parallel`: concurrency limit (default 4)
  *   - `k_value`: required successes for k_of_n policy (default 1)
  *
  * Stores results in context under `parallel.results` as a comma-separated
  * string of `nodeId:status` pairs for downstream fan-in consumption.
  */
class ParallelHandler(registry: HandlerRegistry) extends Handler:

  def execute(node: Node, context: Context, graph: Graph, logsRoot: String): IO[Outcome] =
    val outEdges = graph.outgoingEdges(node.id)
    val joinPolicy = parseJoinPolicy(node.attributes.getOrElse("join_policy", "wait_all"))
    val errorPolicy = parseErrorPolicy(node.attributes.getOrElse("error_policy", "fail_fast"))
    val maxParallel = node.attributes.get("max_parallel").flatMap(_.toIntOption).getOrElse(4)
    val kValue = node.attributes.get("k_value").flatMap(_.toIntOption).getOrElse(1)

    // Build branch tasks: each gets its own cloned context
    val branchesIO: IO[List[(String, IO[Outcome])]] = outEdges.traverse: edge =>
      context.clone.map: branchCtx =>
        val targetNode = graph.nodes.getOrElse(edge.to, Node(edge.to))
        val handler = registry.resolve(targetNode)
        edge.to -> handler.execute(targetNode, branchCtx, graph, logsRoot)

    branchesIO.flatMap: branches =>
      if branches.isEmpty then
        IO.pure(Outcome.success(notes = s"Parallel node '${node.id}' has no outgoing edges"))
      else
        // Execute with bounded parallelism
        branches
          .map((id, task) => task.attempt.map(id -> _))
          .parTraverseN(maxParallel)(identity)
          .flatMap: results =>
            val outcomes: List[(String, Outcome)] = results.map:
              case (id, Right(outcome)) => id -> outcome
              case (id, Left(err))      => id -> Outcome.fail(failureReason = err.getMessage)

            evaluateJoinPolicy(node, outcomes, joinPolicy, errorPolicy, kValue)

  private def evaluateJoinPolicy(
      node: Node,
      outcomes: List[(String, Outcome)],
      joinPolicy: JoinPolicy,
      errorPolicy: ErrorPolicy,
      kValue: Int
  ): IO[Outcome] =
    val successes = outcomes.filter(_._2.status == StageStatus.Success)
    val failures = outcomes.filter(_._2.status == StageStatus.Fail)
    val resultsStr = outcomes.map((id, o) => s"$id:${o.status}").mkString(",")

    val status = joinPolicy match
      case JoinPolicy.WaitAll =>
        if failures.isEmpty then StageStatus.Success
        else errorPolicy match
          case ErrorPolicy.Ignore   => StageStatus.Success
          case ErrorPolicy.Continue => StageStatus.Success
          case ErrorPolicy.FailFast => StageStatus.Fail

      case JoinPolicy.FirstSuccess =>
        if successes.nonEmpty then StageStatus.Success
        else StageStatus.Fail

      case JoinPolicy.KOfN =>
        if successes.size >= kValue then StageStatus.Success
        else StageStatus.Fail

      case JoinPolicy.Quorum =>
        if successes.size > outcomes.size / 2 then StageStatus.Success
        else StageStatus.Fail

    val notes = s"Parallel '${node.id}': ${successes.size}/${outcomes.size} succeeded"
    IO.pure(Outcome(
      status = status,
      notes = notes,
      contextUpdates = Map("parallel.results" -> resultsStr)
    ))

  private def parseJoinPolicy(s: String): JoinPolicy = s.toLowerCase match
    case "first_success" => JoinPolicy.FirstSuccess
    case "k_of_n"        => JoinPolicy.KOfN
    case "quorum"        => JoinPolicy.Quorum
    case _               => JoinPolicy.WaitAll

  private def parseErrorPolicy(s: String): ErrorPolicy = s.toLowerCase match
    case "continue" => ErrorPolicy.Continue
    case "ignore"   => ErrorPolicy.Ignore
    case _          => ErrorPolicy.FailFast
