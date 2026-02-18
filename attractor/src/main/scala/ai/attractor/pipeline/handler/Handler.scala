package ai.attractor.pipeline.handler

import cats.effect.IO

import ai.attractor.pipeline.parser.{Graph, Node}
import ai.attractor.pipeline.state.{Context, Outcome, StageStatus}

// ---------------------------------------------------------------------------
// Handler trait and registry for pipeline node execution
// ---------------------------------------------------------------------------

/** A handler knows how to execute a specific type of pipeline node. */
trait Handler:
  def execute(node: Node, context: Context, graph: Graph, logsRoot: String): IO[Outcome]

/** Registry that maps node types/shapes to their handlers.
  *
  * Resolution order:
  *   1. Explicit `type` attribute on the node
  *   2. Shape-based lookup via the shapeToType mapping
  *   3. Fall back to the default handler
  */
class HandlerRegistry(
    private val handlers: scala.collection.mutable.Map[String, Handler] =
      scala.collection.mutable.Map.empty,
    val defaultHandler: Handler
):

  def register(typeString: String, handler: Handler): Unit =
    handlers(typeString) = handler

  def resolve(node: Node): Handler =
    // 1. Explicit type attribute
    if node.nodeType.nonEmpty && handlers.contains(node.nodeType) then
      handlers(node.nodeType)
    // 2. Shape-based resolution
    else
      val handlerType = shapeToType.getOrElse(node.shape, "")
      if handlerType.nonEmpty && handlers.contains(handlerType) then
        handlers(handlerType)
      // 3. Default
      else defaultHandler

  private val shapeToType: Map[String, String] = Map(
    "Mdiamond" -> "start",
    "Msquare" -> "exit",
    "box" -> "codergen",
    "hexagon" -> "wait.human",
    "diamond" -> "conditional",
    "component" -> "parallel",
    "tripleoctagon" -> "parallel.fan_in",
    "parallelogram" -> "tool",
    "house" -> "stack.manager_loop"
  )
