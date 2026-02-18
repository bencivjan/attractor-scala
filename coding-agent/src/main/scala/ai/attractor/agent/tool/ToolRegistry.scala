package ai.attractor.agent.tool

import ai.attractor.llm.model.ToolDefinition
import ai.attractor.agent.env.ExecutionEnvironment
import io.circe.Json

case class RegisteredTool[F[_]](
    definition: ToolDefinition,
    executor: (Json, ExecutionEnvironment[F]) => F[String]
)

class ToolRegistry[F[_]]:
  private val tools = scala.collection.mutable.Map[String, RegisteredTool[F]]()

  def register(tool: RegisteredTool[F]): Unit =
    tools(tool.definition.name) = tool

  def unregister(name: String): Unit =
    tools.remove(name)

  def get(name: String): Option[RegisteredTool[F]] =
    tools.get(name)

  def definitions: List[ToolDefinition] =
    tools.values.map(_.definition).toList

  def names: List[String] =
    tools.keys.toList

  def registerAll(newTools: List[RegisteredTool[F]]): Unit =
    newTools.foreach(register)

object ToolRegistry:
  def empty[F[_]]: ToolRegistry[F] = new ToolRegistry[F]
