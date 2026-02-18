package ai.attractor.pipeline.handler

import cats.effect.IO

import ai.attractor.pipeline.parser.{Graph, Node}
import ai.attractor.pipeline.state.{Context, Outcome}

// ---------------------------------------------------------------------------
// Start node handler -- no-op pass-through
// ---------------------------------------------------------------------------

/** The start node simply marks the entry point of the pipeline.
  * It returns Success immediately with no side effects.
  */
object StartHandler extends Handler:
  def execute(node: Node, context: Context, graph: Graph, logsRoot: String): IO[Outcome] =
    IO.pure(Outcome.success(notes = s"Start node '${node.id}' entered"))
