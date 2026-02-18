package ai.attractor.agent.session

import ai.attractor.llm.model.*

enum SessionState:
  case Idle, Processing, AwaitingInput, Closed

case class SessionConfig(
    maxTurns: Int = 0,
    maxToolRoundsPerInput: Int = 0,
    defaultCommandTimeoutMs: Int = 10000,
    maxCommandTimeoutMs: Int = 600000,
    reasoningEffort: Option[String] = None,
    toolOutputLimits: Map[String, Int] = Map.empty,
    toolLineLimits: Map[String, Int] = Map.empty,
    enableLoopDetection: Boolean = true,
    loopDetectionWindow: Int = 10,
    maxSubagentDepth: Int = 1
)

// Turn types
sealed trait Turn:
  def timestamp: java.time.Instant

case class UserTurn(
    content: String,
    timestamp: java.time.Instant = java.time.Instant.now()
) extends Turn

case class AssistantTurn(
    content: String,
    toolCalls: List[ToolCall],
    reasoning: Option[String],
    usage: Usage,
    responseId: Option[String],
    timestamp: java.time.Instant = java.time.Instant.now()
) extends Turn

case class ToolResultsTurn(
    results: List[ToolResult],
    timestamp: java.time.Instant = java.time.Instant.now()
) extends Turn

case class SystemTurn(
    content: String,
    timestamp: java.time.Instant = java.time.Instant.now()
) extends Turn

case class SteeringTurn(
    content: String,
    timestamp: java.time.Instant = java.time.Instant.now()
) extends Turn
