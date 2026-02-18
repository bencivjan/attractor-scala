package ai.attractor.pipeline.handler

import cats.effect.IO

import ai.attractor.pipeline.parser.{Graph, Node}
import ai.attractor.pipeline.state.{Context, Outcome, StageStatus}

// ---------------------------------------------------------------------------
// Fan-in handler -- aggregates parallel branch results
// ---------------------------------------------------------------------------

/** Reads `parallel.results` from context (written by ParallelHandler),
  * selects the best candidate by heuristic:
  *   1. Rank by status: Success > Fail > other
  *   2. Break ties lexically by node id
  *
  * Stores the winning branch id in `parallel.best_branch`.
  */
object FanInHandler extends Handler:

  def execute(node: Node, context: Context, graph: Graph, logsRoot: String): IO[Outcome] =
    context.getString("parallel.results").flatMap: resultsRaw =>
      if resultsRaw.isEmpty then
        IO.pure(Outcome.success(
          notes = s"Fan-in '${node.id}': no parallel results found",
          contextUpdates = Map("parallel.best_branch" -> "")
        ))
      else
        val entries = resultsRaw.split(",").toList.flatMap: entry =>
          entry.split(":").toList match
            case id :: statusStr :: Nil => Some(id -> statusStr)
            case _                      => None

        val sorted = entries.sortBy: (id, statusStr) =>
          val rank = statusStr.toLowerCase match
            case "success" => 0
            case "fail"    => 1
            case _         => 2
          (rank, id)

        val bestBranch = sorted.headOption.map(_._1).getOrElse("")
        val allSucceeded = entries.forall(_._2.equalsIgnoreCase("success"))

        IO.pure(Outcome(
          status = if allSucceeded then StageStatus.Success else StageStatus.Success,
          notes = s"Fan-in '${node.id}': selected best branch '$bestBranch' from ${entries.size} branches",
          contextUpdates = Map(
            "parallel.best_branch" -> bestBranch,
            "last_stage" -> node.id
          )
        ))
