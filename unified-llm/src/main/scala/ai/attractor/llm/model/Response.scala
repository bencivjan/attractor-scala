package ai.attractor.llm.model

import io.circe.*
import io.circe.syntax.*

final case class Response(
    id: String,
    model: String,
    provider: String,
    message: Message,
    finishReason: FinishReason,
    usage: Usage,
    raw: Option[Json] = None,
    warnings: List[Warning] = Nil,
    rateLimit: Option[RateLimitInfo] = None
):
  /** Convenience: extract concatenated text from the response message. */
  def text: String = message.text

  /** Convenience: extract all tool calls from the response message content. */
  def toolCalls: List[ToolCall] =
    message.content.collect:
      case cp if cp.kind == ContentKind.ToolCall =>
        cp.toolCall.map(tc => ToolCall(tc.id, tc.name, tc.arguments)).toList
    .flatten

  /** Convenience: extract reasoning / thinking text from the response message. */
  def reasoning: Option[String] =
    val parts = message.content.collect:
      case cp if cp.kind == ContentKind.Thinking =>
        cp.thinking.map(_.text).getOrElse("")
    Option.when(parts.nonEmpty)(parts.mkString)

object Response:
  given Encoder[Response] = Encoder.instance: r =>
    Json.obj(
      "id" -> r.id.asJson,
      "model" -> r.model.asJson,
      "provider" -> r.provider.asJson,
      "message" -> r.message.asJson,
      "finish_reason" -> r.finishReason.asJson,
      "usage" -> r.usage.asJson,
      "raw" -> r.raw.asJson,
      "warnings" -> (if r.warnings.isEmpty then None else Some(r.warnings)).asJson,
      "rate_limit" -> r.rateLimit.asJson
    ).dropNullValues

  given Decoder[Response] = Decoder.instance: c =>
    for
      id           <- c.downField("id").as[String]
      model        <- c.downField("model").as[String]
      provider     <- c.downField("provider").as[String]
      message      <- c.downField("message").as[Message]
      finishReason <- c.downField("finish_reason").as[FinishReason]
      usage        <- c.downField("usage").as[Usage]
      raw          <- c.downField("raw").as[Option[Json]]
      warnings     <- c.downField("warnings").as[Option[List[Warning]]]
      rateLimit    <- c.downField("rate_limit").as[Option[RateLimitInfo]]
    yield Response(
      id, model, provider, message, finishReason, usage,
      raw, warnings.getOrElse(Nil), rateLimit
    )
