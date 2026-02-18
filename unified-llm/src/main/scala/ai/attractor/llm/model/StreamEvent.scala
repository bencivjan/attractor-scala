package ai.attractor.llm.model

import io.circe.*
import io.circe.syntax.*

// ---------------------------------------------------------------------------
// StreamEventType
// ---------------------------------------------------------------------------

enum StreamEventType:
  case StreamStart
  case ContentStart
  case ContentDelta
  case ContentEnd
  case ToolCallStart
  case ToolCallDelta
  case ToolCallEnd
  case ThinkingStart
  case ThinkingDelta
  case ThinkingEnd
  case UsageUpdate
  case StreamEnd
  case Error

object StreamEventType:
  given Encoder[StreamEventType] = Encoder[String].contramap:
    case StreamEventType.StreamStart    => "stream_start"
    case StreamEventType.ContentStart   => "content_start"
    case StreamEventType.ContentDelta   => "content_delta"
    case StreamEventType.ContentEnd     => "content_end"
    case StreamEventType.ToolCallStart  => "tool_call_start"
    case StreamEventType.ToolCallDelta  => "tool_call_delta"
    case StreamEventType.ToolCallEnd    => "tool_call_end"
    case StreamEventType.ThinkingStart  => "thinking_start"
    case StreamEventType.ThinkingDelta  => "thinking_delta"
    case StreamEventType.ThinkingEnd    => "thinking_end"
    case StreamEventType.UsageUpdate    => "usage_update"
    case StreamEventType.StreamEnd      => "stream_end"
    case StreamEventType.Error          => "error"

  given Decoder[StreamEventType] = Decoder[String].emap:
    case "stream_start"    => Right(StreamEventType.StreamStart)
    case "content_start"   => Right(StreamEventType.ContentStart)
    case "content_delta"   => Right(StreamEventType.ContentDelta)
    case "content_end"     => Right(StreamEventType.ContentEnd)
    case "tool_call_start" => Right(StreamEventType.ToolCallStart)
    case "tool_call_delta" => Right(StreamEventType.ToolCallDelta)
    case "tool_call_end"   => Right(StreamEventType.ToolCallEnd)
    case "thinking_start"  => Right(StreamEventType.ThinkingStart)
    case "thinking_delta"  => Right(StreamEventType.ThinkingDelta)
    case "thinking_end"    => Right(StreamEventType.ThinkingEnd)
    case "usage_update"    => Right(StreamEventType.UsageUpdate)
    case "stream_end"      => Right(StreamEventType.StreamEnd)
    case "error"           => Right(StreamEventType.Error)
    case other             => Left(s"Unknown stream event type: $other")

// ---------------------------------------------------------------------------
// StreamEvent
// ---------------------------------------------------------------------------

final case class StreamEvent(
    eventType: StreamEventType,
    contentDelta: Option[String] = None,
    toolCallId: Option[String] = None,
    toolCallName: Option[String] = None,
    argumentsDelta: Option[String] = None,
    thinkingDelta: Option[String] = None,
    thinkingSignature: Option[String] = None,
    finishReason: Option[FinishReason] = None,
    usage: Option[Usage] = None,
    model: Option[String] = None,
    responseId: Option[String] = None,
    error: Option[String] = None,
    raw: Option[Json] = None
)

object StreamEvent:
  given Encoder[StreamEvent] = Encoder.instance: se =>
    Json.obj(
      "event_type" -> se.eventType.asJson,
      "content_delta" -> se.contentDelta.asJson,
      "tool_call_id" -> se.toolCallId.asJson,
      "tool_call_name" -> se.toolCallName.asJson,
      "arguments_delta" -> se.argumentsDelta.asJson,
      "thinking_delta" -> se.thinkingDelta.asJson,
      "thinking_signature" -> se.thinkingSignature.asJson,
      "finish_reason" -> se.finishReason.asJson,
      "usage" -> se.usage.asJson,
      "model" -> se.model.asJson,
      "response_id" -> se.responseId.asJson,
      "error" -> se.error.asJson,
      "raw" -> se.raw.asJson
    ).dropNullValues

  given Decoder[StreamEvent] = Decoder.instance: c =>
    for
      eventType         <- c.downField("event_type").as[StreamEventType]
      contentDelta      <- c.downField("content_delta").as[Option[String]]
      toolCallId        <- c.downField("tool_call_id").as[Option[String]]
      toolCallName      <- c.downField("tool_call_name").as[Option[String]]
      argumentsDelta    <- c.downField("arguments_delta").as[Option[String]]
      thinkingDelta     <- c.downField("thinking_delta").as[Option[String]]
      thinkingSignature <- c.downField("thinking_signature").as[Option[String]]
      finishReason      <- c.downField("finish_reason").as[Option[FinishReason]]
      usage             <- c.downField("usage").as[Option[Usage]]
      model             <- c.downField("model").as[Option[String]]
      responseId        <- c.downField("response_id").as[Option[String]]
      error             <- c.downField("error").as[Option[String]]
      raw               <- c.downField("raw").as[Option[Json]]
    yield StreamEvent(
      eventType, contentDelta, toolCallId, toolCallName,
      argumentsDelta, thinkingDelta, thinkingSignature,
      finishReason, usage, model, responseId, error, raw
    )
