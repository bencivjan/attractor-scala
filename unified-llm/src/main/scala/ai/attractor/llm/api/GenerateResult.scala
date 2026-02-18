package ai.attractor.llm.api

import ai.attractor.llm.model.*
import io.circe.Json

/** Result of a single step within a multi-step tool-use loop. */
final case class StepResult(
    text: String,
    reasoning: Option[String],
    toolCalls: List[ToolCall],
    toolResults: List[ToolResult],
    finishReason: FinishReason,
    usage: Usage,
    response: Response,
    warnings: List[Warning] = Nil
)

/** Aggregated result of a complete generate() invocation, possibly spanning
  * multiple tool-use rounds.
  */
final case class GenerateResult(
    text: String,
    reasoning: Option[String],
    toolCalls: List[ToolCall],
    toolResults: List[ToolResult],
    finishReason: FinishReason,
    usage: Usage,
    totalUsage: Usage,
    steps: List[StepResult],
    response: Response,
    output: Option[Json] = None
)
