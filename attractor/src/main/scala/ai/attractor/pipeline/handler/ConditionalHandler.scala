package ai.attractor.pipeline.handler

import cats.effect.IO

import ai.attractor.pipeline.parser.{Graph, Node}
import ai.attractor.pipeline.state.{Context, Outcome}

// ---------------------------------------------------------------------------
// Conditional node handler -- no-op pass-through
// ---------------------------------------------------------------------------

/** Conditional (diamond) nodes are routing points. The actual edge evaluation
  * and branching logic is handled by the engine's edge selection algorithm,
  * so this handler simply returns Success to let the engine proceed.
  */
object ConditionalHandler extends Handler:
  def execute(node: Node, context: Context, graph: Graph, logsRoot: String): IO[Outcome] =
    IO.pure(Outcome.success(notes = s"Conditional node '${node.id}' evaluated"))
