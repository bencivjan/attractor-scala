package ai.attractor.llm.api

import ai.attractor.llm.model.*
import io.circe.Json

/** Mutable accumulator that processes a sequence of StreamEvents and builds
  * up a complete Response.
  *
  * This class is NOT thread-safe; it is intended to be driven sequentially
  * from a single fs2 stream fold or scan. Mutable state is used deliberately
  * here because the accumulator is a short-lived utility whose lifecycle is
  * scoped to a single stream consumption.
  */
final class StreamAccumulator:

  // -- Accumulated state ----------------------------------------------------
  private var _responseId: Option[String]       = None
  private var _model: Option[String]            = None
  private var _finishReason: Option[FinishReason] = None
  private var _usage: Option[Usage]             = None
  private val _textBuilder: StringBuilder       = new StringBuilder
  private val _reasoningBuilder: StringBuilder  = new StringBuilder
  private var _toolCallBuilders: Map[String, ToolCallBuilder] = Map.empty
  private var _toolCallOrder: List[String]      = Nil

  // -- Internal helper for incrementally-built tool calls -------------------
  private final class ToolCallBuilder(
      val id: String,
      var name: String,
      val argsBuilder: StringBuilder = new StringBuilder
  ):
    def toToolCall: ToolCall =
      val argsJson = io.circe.parser.parse(argsBuilder.toString).getOrElse(Json.fromString(argsBuilder.toString))
      ToolCall(id, name, argsJson, rawArguments = Some(argsBuilder.toString))

  // -- Public API -----------------------------------------------------------

  /** Process a single stream event, updating internal state. */
  def process(event: StreamEvent): Unit =
    event.responseId.foreach(id => _responseId = Some(id))
    event.model.foreach(m => _model = Some(m))

    event.eventType match
      case StreamEventType.ContentDelta =>
        event.contentDelta.foreach(_textBuilder.append)

      case StreamEventType.ThinkingDelta =>
        event.thinkingDelta.foreach(_reasoningBuilder.append)

      case StreamEventType.ToolCallStart =>
        val id = event.toolCallId.getOrElse("")
        val name = event.toolCallName.getOrElse("")
        _toolCallBuilders = _toolCallBuilders.updated(id, ToolCallBuilder(id, name))
        _toolCallOrder = _toolCallOrder :+ id

      case StreamEventType.ToolCallDelta =>
        event.toolCallId.foreach: id =>
          _toolCallBuilders.get(id).foreach: builder =>
            event.argumentsDelta.foreach(builder.argsBuilder.append)

      case StreamEventType.UsageUpdate =>
        event.usage.foreach(u => _usage = Some(u))

      case StreamEventType.StreamEnd =>
        event.finishReason.foreach(fr => _finishReason = Some(fr))
        event.usage.foreach(u => _usage = Some(u))

      case _ => () // StreamStart, ContentStart, ContentEnd, ToolCallEnd, ThinkingStart, ThinkingEnd, Error handled elsewhere

  /** Build the final Response from accumulated state, if sufficient data is
    * available (at minimum a finish reason or some content).
    */
  def response: Option[Response] =
    val text = currentText
    val calls = toolCalls
    val reasoning = currentReasoning

    // Only produce a response if we have some content or a finish signal.
    if text.isEmpty && calls.isEmpty && _finishReason.isEmpty then None
    else
      val contentParts = List.concat(
        reasoning.map(r => ContentPart.thinking(ThinkingData(r))).toList,
        Option.when(text.nonEmpty)(ContentPart.text(text)).toList,
        calls.map(tc => ContentPart.toolCall(ToolCallData(tc.id, tc.name, tc.arguments)))
      )

      val message = Message(Role.Assistant, contentParts)

      Some(Response(
        id = _responseId.getOrElse(""),
        model = _model.getOrElse(""),
        provider = "",
        message = message,
        finishReason = _finishReason.getOrElse(FinishReason.stop),
        usage = _usage.getOrElse(Usage.empty)
      ))

  /** The text accumulated so far from content deltas. */
  def currentText: String =
    _textBuilder.toString

  /** The reasoning text accumulated so far, or None if no thinking events
    * were received.
    */
  def currentReasoning: Option[String] =
    val s = _reasoningBuilder.toString
    Option.when(s.nonEmpty)(s)

  /** All tool calls accumulated so far, in order of first appearance. */
  def toolCalls: List[ToolCall] =
    _toolCallOrder.flatMap(id => _toolCallBuilders.get(id).map(_.toToolCall))
