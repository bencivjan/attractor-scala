package ai.attractor.pipeline.handler

import cats.effect.IO

import ai.attractor.pipeline.parser.{Graph, Node}
import ai.attractor.pipeline.state.{Context, Outcome}

// ---------------------------------------------------------------------------
// Exit node handler -- no-op terminal
// ---------------------------------------------------------------------------

/** The exit node marks the end of the pipeline.
  * It returns Success immediately with no side effects.
  */
object ExitHandler extends Handler:
  def execute(node: Node, context: Context, graph: Graph, logsRoot: String): IO[Outcome] =
    IO.pure(Outcome.success(notes = s"Exit node '${node.id}' reached"))
