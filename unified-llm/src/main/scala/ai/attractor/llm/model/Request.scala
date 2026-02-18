package ai.attractor.llm.model

import io.circe.*
import io.circe.syntax.*

final case class Request(
    model: String,
    messages: List[Message],
    provider: Option[String] = None,
    tools: List[ToolDefinition] = Nil,
    toolChoice: Option[ToolChoice] = None,
    responseFormat: Option[ResponseFormat] = None,
    temperature: Option[Double] = None,
    topP: Option[Double] = None,
    maxTokens: Option[Int] = None,
    stopSequences: List[String] = Nil,
    reasoningEffort: Option[String] = None,
    metadata: Option[Json] = None,
    providerOptions: Option[Json] = None
)

object Request:
  given Encoder[Request] = Encoder.instance: r =>
    Json.obj(
      "model" -> r.model.asJson,
      "messages" -> r.messages.asJson,
      "provider" -> r.provider.asJson,
      "tools" -> (if r.tools.isEmpty then None else Some(r.tools)).asJson,
      "tool_choice" -> r.toolChoice.asJson,
      "response_format" -> r.responseFormat.asJson,
      "temperature" -> r.temperature.asJson,
      "top_p" -> r.topP.asJson,
      "max_tokens" -> r.maxTokens.asJson,
      "stop_sequences" -> (if r.stopSequences.isEmpty then None else Some(r.stopSequences)).asJson,
      "reasoning_effort" -> r.reasoningEffort.asJson,
      "metadata" -> r.metadata.asJson,
      "provider_options" -> r.providerOptions.asJson
    ).dropNullValues

  given Decoder[Request] = Decoder.instance: c =>
    for
      model           <- c.downField("model").as[String]
      messages        <- c.downField("messages").as[List[Message]]
      provider        <- c.downField("provider").as[Option[String]]
      tools           <- c.downField("tools").as[Option[List[ToolDefinition]]]
      toolChoice      <- c.downField("tool_choice").as[Option[ToolChoice]]
      responseFormat  <- c.downField("response_format").as[Option[ResponseFormat]]
      temperature     <- c.downField("temperature").as[Option[Double]]
      topP            <- c.downField("top_p").as[Option[Double]]
      maxTokens       <- c.downField("max_tokens").as[Option[Int]]
      stopSequences   <- c.downField("stop_sequences").as[Option[List[String]]]
      reasoningEffort <- c.downField("reasoning_effort").as[Option[String]]
      metadata        <- c.downField("metadata").as[Option[Json]]
      providerOptions <- c.downField("provider_options").as[Option[Json]]
    yield Request(
      model, messages, provider,
      tools.getOrElse(Nil), toolChoice, responseFormat,
      temperature, topP, maxTokens,
      stopSequences.getOrElse(Nil), reasoningEffort,
      metadata, providerOptions
    )
