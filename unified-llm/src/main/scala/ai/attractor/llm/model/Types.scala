package ai.attractor.llm.model

import io.circe.*
import io.circe.syntax.*
import io.circe.generic.semiauto.*

// ---------------------------------------------------------------------------
// Role
// ---------------------------------------------------------------------------

enum Role:
  case System, User, Assistant, Tool, Developer

object Role:
  given Encoder[Role] = Encoder[String].contramap(_.toString.toLowerCase)
  given Decoder[Role] = Decoder[String].emap: s =>
    s.toLowerCase match
      case "system"    => Right(Role.System)
      case "user"      => Right(Role.User)
      case "assistant" => Right(Role.Assistant)
      case "tool"      => Right(Role.Tool)
      case "developer" => Right(Role.Developer)
      case other       => Left(s"Unknown role: $other")

// ---------------------------------------------------------------------------
// ContentKind
// ---------------------------------------------------------------------------

enum ContentKind:
  case Text, Image, Audio, Document, ToolCall, ToolResult, Thinking, RedactedThinking

object ContentKind:
  given Encoder[ContentKind] = Encoder[String].contramap:
    case ContentKind.RedactedThinking => "redacted_thinking"
    case other                        => other.toString.toLowerCase

  given Decoder[ContentKind] = Decoder[String].emap: s =>
    s.toLowerCase match
      case "text"              => Right(ContentKind.Text)
      case "image"             => Right(ContentKind.Image)
      case "audio"             => Right(ContentKind.Audio)
      case "document"          => Right(ContentKind.Document)
      case "tool_call"         => Right(ContentKind.ToolCall)
      case "tool_result"       => Right(ContentKind.ToolResult)
      case "thinking"          => Right(ContentKind.Thinking)
      case "redacted_thinking" => Right(ContentKind.RedactedThinking)
      case other               => Left(s"Unknown content kind: $other")

// ---------------------------------------------------------------------------
// Data carriers for content parts
// ---------------------------------------------------------------------------

final case class ImageData(
    url: Option[String] = None,
    data: Option[Array[Byte]] = None,
    mediaType: Option[String] = None,
    detail: Option[String] = None
)

object ImageData:
  // Array[Byte] needs custom codec because circe has no built-in derivation
  given Encoder[ImageData] = Encoder.instance: d =>
    Json.obj(
      "url" -> d.url.asJson,
      "data" -> d.data.map(java.util.Base64.getEncoder.encodeToString).asJson,
      "media_type" -> d.mediaType.asJson,
      "detail" -> d.detail.asJson
    ).dropNullValues

  given Decoder[ImageData] = Decoder.instance: c =>
    for
      url       <- c.downField("url").as[Option[String]]
      dataB64   <- c.downField("data").as[Option[String]]
      mediaType <- c.downField("media_type").as[Option[String]]
      detail    <- c.downField("detail").as[Option[String]]
    yield ImageData(url, dataB64.map(java.util.Base64.getDecoder.decode), mediaType, detail)

final case class AudioData(
    url: Option[String] = None,
    data: Option[Array[Byte]] = None,
    mediaType: Option[String] = None
)

object AudioData:
  given Encoder[AudioData] = Encoder.instance: d =>
    Json.obj(
      "url" -> d.url.asJson,
      "data" -> d.data.map(java.util.Base64.getEncoder.encodeToString).asJson,
      "media_type" -> d.mediaType.asJson
    ).dropNullValues

  given Decoder[AudioData] = Decoder.instance: c =>
    for
      url       <- c.downField("url").as[Option[String]]
      dataB64   <- c.downField("data").as[Option[String]]
      mediaType <- c.downField("media_type").as[Option[String]]
    yield AudioData(url, dataB64.map(java.util.Base64.getDecoder.decode), mediaType)

final case class DocumentData(
    url: Option[String] = None,
    data: Option[Array[Byte]] = None,
    mediaType: Option[String] = None,
    fileName: Option[String] = None
)

object DocumentData:
  given Encoder[DocumentData] = Encoder.instance: d =>
    Json.obj(
      "url" -> d.url.asJson,
      "data" -> d.data.map(java.util.Base64.getEncoder.encodeToString).asJson,
      "media_type" -> d.mediaType.asJson,
      "file_name" -> d.fileName.asJson
    ).dropNullValues

  given Decoder[DocumentData] = Decoder.instance: c =>
    for
      url       <- c.downField("url").as[Option[String]]
      dataB64   <- c.downField("data").as[Option[String]]
      mediaType <- c.downField("media_type").as[Option[String]]
      fileName  <- c.downField("file_name").as[Option[String]]
    yield DocumentData(url, dataB64.map(java.util.Base64.getDecoder.decode), mediaType, fileName)

final case class ToolCallData(
    id: String,
    name: String,
    arguments: Json,
    toolType: String = "function"
)

object ToolCallData:
  given Encoder[ToolCallData] = deriveEncoder[ToolCallData]
  given Decoder[ToolCallData] = Decoder.instance: c =>
    for
      id        <- c.downField("id").as[String]
      name      <- c.downField("name").as[String]
      arguments <- c.downField("arguments").as[Json]
      toolType  <- c.downField("toolType").as[Option[String]]
    yield ToolCallData(id, name, arguments, toolType.getOrElse("function"))

final case class ToolResultData(
    toolCallId: String,
    content: Json,
    isError: Boolean = false,
    imageData: Option[Array[Byte]] = None,
    imageMediaType: Option[String] = None
)

object ToolResultData:
  given Encoder[ToolResultData] = Encoder.instance: d =>
    Json.obj(
      "tool_call_id" -> d.toolCallId.asJson,
      "content" -> d.content.asJson,
      "is_error" -> d.isError.asJson,
      "image_data" -> d.imageData.map(java.util.Base64.getEncoder.encodeToString).asJson,
      "image_media_type" -> d.imageMediaType.asJson
    ).dropNullValues

  given Decoder[ToolResultData] = Decoder.instance: c =>
    for
      toolCallId     <- c.downField("tool_call_id").as[String]
      content        <- c.downField("content").as[Json]
      isError        <- c.downField("is_error").as[Option[Boolean]]
      imageDataB64   <- c.downField("image_data").as[Option[String]]
      imageMediaType <- c.downField("image_media_type").as[Option[String]]
    yield ToolResultData(
      toolCallId,
      content,
      isError.getOrElse(false),
      imageDataB64.map(java.util.Base64.getDecoder.decode),
      imageMediaType
    )

final case class ThinkingData(
    text: String,
    signature: Option[String] = None,
    redacted: Boolean = false
)

object ThinkingData:
  given Encoder[ThinkingData] = deriveEncoder[ThinkingData]
  given Decoder[ThinkingData] = Decoder.instance: c =>
    for
      text      <- c.downField("text").as[String]
      signature <- c.downField("signature").as[Option[String]]
      redacted  <- c.downField("redacted").as[Option[Boolean]]
    yield ThinkingData(text, signature, redacted.getOrElse(false))

// ---------------------------------------------------------------------------
// ContentPart - tagged union using kind discriminator
// ---------------------------------------------------------------------------

final case class ContentPart(
    kind: ContentKind,
    text: Option[String] = None,
    image: Option[ImageData] = None,
    audio: Option[AudioData] = None,
    document: Option[DocumentData] = None,
    toolCall: Option[ToolCallData] = None,
    toolResult: Option[ToolResultData] = None,
    thinking: Option[ThinkingData] = None
)

object ContentPart:
  // Factory helpers
  def text(value: String): ContentPart =
    ContentPart(kind = ContentKind.Text, text = Some(value))

  def image(data: ImageData): ContentPart =
    ContentPart(kind = ContentKind.Image, image = Some(data))

  def audio(data: AudioData): ContentPart =
    ContentPart(kind = ContentKind.Audio, audio = Some(data))

  def document(data: DocumentData): ContentPart =
    ContentPart(kind = ContentKind.Document, document = Some(data))

  def toolCall(data: ToolCallData): ContentPart =
    ContentPart(kind = ContentKind.ToolCall, toolCall = Some(data))

  def toolResult(data: ToolResultData): ContentPart =
    ContentPart(kind = ContentKind.ToolResult, toolResult = Some(data))

  def thinking(data: ThinkingData): ContentPart =
    ContentPart(kind = ContentKind.Thinking, thinking = Some(data))

  def redactedThinking(data: ThinkingData): ContentPart =
    ContentPart(kind = ContentKind.RedactedThinking, thinking = Some(data.copy(redacted = true)))

  given Encoder[ContentPart] = Encoder.instance: cp =>
    val base = Json.obj("kind" -> cp.kind.asJson)
    val fields = cp.kind match
      case ContentKind.Text             => Json.obj("text" -> cp.text.asJson)
      case ContentKind.Image            => Json.obj("image" -> cp.image.asJson)
      case ContentKind.Audio            => Json.obj("audio" -> cp.audio.asJson)
      case ContentKind.Document         => Json.obj("document" -> cp.document.asJson)
      case ContentKind.ToolCall         => Json.obj("tool_call" -> cp.toolCall.asJson)
      case ContentKind.ToolResult       => Json.obj("tool_result" -> cp.toolResult.asJson)
      case ContentKind.Thinking         => Json.obj("thinking" -> cp.thinking.asJson)
      case ContentKind.RedactedThinking => Json.obj("thinking" -> cp.thinking.asJson)
    base.deepMerge(fields)

  given Decoder[ContentPart] = Decoder.instance: c =>
    for
      kind       <- c.downField("kind").as[ContentKind]
      text       <- c.downField("text").as[Option[String]]
      image      <- c.downField("image").as[Option[ImageData]]
      audio      <- c.downField("audio").as[Option[AudioData]]
      document   <- c.downField("document").as[Option[DocumentData]]
      toolCall   <- c.downField("tool_call").as[Option[ToolCallData]]
      toolResult <- c.downField("tool_result").as[Option[ToolResultData]]
      thinking   <- c.downField("thinking").as[Option[ThinkingData]]
    yield ContentPart(kind, text, image, audio, document, toolCall, toolResult, thinking)

// ---------------------------------------------------------------------------
// Message
// ---------------------------------------------------------------------------

final case class Message(
    role: Role,
    content: List[ContentPart],
    name: Option[String] = None,
    toolCallId: Option[String] = None
):
  /** Extract concatenated text from all Text content parts. */
  def text: String =
    content.collect { case cp if cp.kind == ContentKind.Text => cp.text.getOrElse("") }.mkString

object Message:
  def system(text: String): Message =
    Message(Role.System, List(ContentPart.text(text)))

  def user(text: String): Message =
    Message(Role.User, List(ContentPart.text(text)))

  def assistant(text: String): Message =
    Message(Role.Assistant, List(ContentPart.text(text)))

  def assistant(parts: List[ContentPart]): Message =
    Message(Role.Assistant, parts)

  def toolResult(toolCallId: String, content: Json, isError: Boolean = false): Message =
    Message(
      Role.Tool,
      List(ContentPart.toolResult(ToolResultData(toolCallId, content, isError))),
      toolCallId = Some(toolCallId)
    )

  given Encoder[Message] = Encoder.instance: m =>
    Json.obj(
      "role" -> m.role.asJson,
      "content" -> m.content.asJson,
      "name" -> m.name.asJson,
      "tool_call_id" -> m.toolCallId.asJson
    ).dropNullValues

  given Decoder[Message] = Decoder.instance: c =>
    for
      role       <- c.downField("role").as[Role]
      content    <- c.downField("content").as[List[ContentPart]]
      name       <- c.downField("name").as[Option[String]]
      toolCallId <- c.downField("tool_call_id").as[Option[String]]
    yield Message(role, content, name, toolCallId)

// ---------------------------------------------------------------------------
// FinishReason
// ---------------------------------------------------------------------------

final case class FinishReason(reason: String, raw: Option[String] = None)

object FinishReason:
  val stop: FinishReason          = FinishReason("stop")
  val length: FinishReason        = FinishReason("length")
  val toolCalls: FinishReason     = FinishReason("tool_calls")
  val contentFilter: FinishReason = FinishReason("content_filter")
  val error: FinishReason         = FinishReason("error")
  val other: FinishReason         = FinishReason("other")

  given Encoder[FinishReason] = Encoder.instance: fr =>
    Json.obj(
      "reason" -> fr.reason.asJson,
      "raw" -> fr.raw.asJson
    ).dropNullValues

  given Decoder[FinishReason] = Decoder.instance: c =>
    for
      reason <- c.downField("reason").as[String]
      raw    <- c.downField("raw").as[Option[String]]
    yield FinishReason(reason, raw)

// ---------------------------------------------------------------------------
// Usage
// ---------------------------------------------------------------------------

final case class Usage(
    inputTokens: Int = 0,
    outputTokens: Int = 0,
    totalTokens: Int = 0,
    reasoningTokens: Option[Int] = None,
    cacheReadTokens: Option[Int] = None,
    cacheWriteTokens: Option[Int] = None,
    raw: Option[Json] = None
):
  def +(that: Usage): Usage =
    Usage(
      inputTokens = this.inputTokens + that.inputTokens,
      outputTokens = this.outputTokens + that.outputTokens,
      totalTokens = this.totalTokens + that.totalTokens,
      reasoningTokens = sumOpt(this.reasoningTokens, that.reasoningTokens),
      cacheReadTokens = sumOpt(this.cacheReadTokens, that.cacheReadTokens),
      cacheWriteTokens = sumOpt(this.cacheWriteTokens, that.cacheWriteTokens),
      raw = this.raw.orElse(that.raw)
    )

  private def sumOpt(a: Option[Int], b: Option[Int]): Option[Int] =
    (a, b) match
      case (Some(x), Some(y)) => Some(x + y)
      case (Some(x), None)    => Some(x)
      case (None, Some(y))    => Some(y)
      case _                  => None

object Usage:
  val empty: Usage = Usage()

  given Encoder[Usage] = Encoder.instance: u =>
    Json.obj(
      "input_tokens" -> u.inputTokens.asJson,
      "output_tokens" -> u.outputTokens.asJson,
      "total_tokens" -> u.totalTokens.asJson,
      "reasoning_tokens" -> u.reasoningTokens.asJson,
      "cache_read_tokens" -> u.cacheReadTokens.asJson,
      "cache_write_tokens" -> u.cacheWriteTokens.asJson,
      "raw" -> u.raw.asJson
    ).dropNullValues

  given Decoder[Usage] = Decoder.instance: c =>
    for
      input     <- c.downField("input_tokens").as[Option[Int]]
      output    <- c.downField("output_tokens").as[Option[Int]]
      total     <- c.downField("total_tokens").as[Option[Int]]
      reasoning <- c.downField("reasoning_tokens").as[Option[Int]]
      cacheR    <- c.downField("cache_read_tokens").as[Option[Int]]
      cacheW    <- c.downField("cache_write_tokens").as[Option[Int]]
      raw       <- c.downField("raw").as[Option[Json]]
    yield Usage(
      input.getOrElse(0),
      output.getOrElse(0),
      total.getOrElse(0),
      reasoning,
      cacheR,
      cacheW,
      raw
    )

// ---------------------------------------------------------------------------
// Warning
// ---------------------------------------------------------------------------

final case class Warning(message: String, code: Option[String] = None)

object Warning:
  given Encoder[Warning] = Encoder.instance: w =>
    Json.obj(
      "message" -> w.message.asJson,
      "code" -> w.code.asJson
    ).dropNullValues

  given Decoder[Warning] = Decoder.instance: c =>
    for
      message <- c.downField("message").as[String]
      code    <- c.downField("code").as[Option[String]]
    yield Warning(message, code)

// ---------------------------------------------------------------------------
// RateLimitInfo
// ---------------------------------------------------------------------------

final case class RateLimitInfo(
    requestLimit: Option[Int] = None,
    requestRemaining: Option[Int] = None,
    requestReset: Option[String] = None,
    tokenLimit: Option[Int] = None,
    tokenRemaining: Option[Int] = None,
    tokenReset: Option[String] = None
)

object RateLimitInfo:
  given Encoder[RateLimitInfo] = Encoder.instance: r =>
    Json.obj(
      "request_limit" -> r.requestLimit.asJson,
      "request_remaining" -> r.requestRemaining.asJson,
      "request_reset" -> r.requestReset.asJson,
      "token_limit" -> r.tokenLimit.asJson,
      "token_remaining" -> r.tokenRemaining.asJson,
      "token_reset" -> r.tokenReset.asJson
    ).dropNullValues

  given Decoder[RateLimitInfo] = Decoder.instance: c =>
    for
      reqLimit <- c.downField("request_limit").as[Option[Int]]
      reqRem   <- c.downField("request_remaining").as[Option[Int]]
      reqReset <- c.downField("request_reset").as[Option[String]]
      tokLimit <- c.downField("token_limit").as[Option[Int]]
      tokRem   <- c.downField("token_remaining").as[Option[Int]]
      tokReset <- c.downField("token_reset").as[Option[String]]
    yield RateLimitInfo(reqLimit, reqRem, reqReset, tokLimit, tokRem, tokReset)

// ---------------------------------------------------------------------------
// ResponseFormat
// ---------------------------------------------------------------------------

final case class ResponseFormat(
    formatType: String,
    jsonSchema: Option[Json] = None,
    strict: Boolean = false
)

object ResponseFormat:
  given Encoder[ResponseFormat] = Encoder.instance: rf =>
    Json.obj(
      "format_type" -> rf.formatType.asJson,
      "json_schema" -> rf.jsonSchema.asJson,
      "strict" -> rf.strict.asJson
    ).dropNullValues

  given Decoder[ResponseFormat] = Decoder.instance: c =>
    for
      ft     <- c.downField("format_type").as[String]
      schema <- c.downField("json_schema").as[Option[Json]]
      strict <- c.downField("strict").as[Option[Boolean]]
    yield ResponseFormat(ft, schema, strict.getOrElse(false))

// ---------------------------------------------------------------------------
// ToolChoice
// ---------------------------------------------------------------------------

final case class ToolChoice(mode: String, toolName: Option[String] = None)

object ToolChoice:
  val auto: ToolChoice     = ToolChoice("auto")
  val none: ToolChoice     = ToolChoice("none")
  val required: ToolChoice = ToolChoice("required")

  def named(name: String): ToolChoice = ToolChoice("named", Some(name))

  given Encoder[ToolChoice] = Encoder.instance: tc =>
    Json.obj(
      "mode" -> tc.mode.asJson,
      "tool_name" -> tc.toolName.asJson
    ).dropNullValues

  given Decoder[ToolChoice] = Decoder.instance: c =>
    for
      mode     <- c.downField("mode").as[String]
      toolName <- c.downField("tool_name").as[Option[String]]
    yield ToolChoice(mode, toolName)

// ---------------------------------------------------------------------------
// ToolDefinition
// ---------------------------------------------------------------------------

final case class ToolDefinition(
    name: String,
    description: String,
    parameters: Json
)

object ToolDefinition:
  given Encoder[ToolDefinition] = deriveEncoder[ToolDefinition]
  given Decoder[ToolDefinition] = deriveDecoder[ToolDefinition]

// ---------------------------------------------------------------------------
// ToolCall
// ---------------------------------------------------------------------------

final case class ToolCall(
    id: String,
    name: String,
    arguments: Json,
    rawArguments: Option[String] = None
)

object ToolCall:
  given Encoder[ToolCall] = Encoder.instance: tc =>
    Json.obj(
      "id" -> tc.id.asJson,
      "name" -> tc.name.asJson,
      "arguments" -> tc.arguments.asJson,
      "raw_arguments" -> tc.rawArguments.asJson
    ).dropNullValues

  given Decoder[ToolCall] = Decoder.instance: c =>
    for
      id      <- c.downField("id").as[String]
      name    <- c.downField("name").as[String]
      args    <- c.downField("arguments").as[Json]
      rawArgs <- c.downField("raw_arguments").as[Option[String]]
    yield ToolCall(id, name, args, rawArgs)

// ---------------------------------------------------------------------------
// ToolResult
// ---------------------------------------------------------------------------

final case class ToolResult(
    toolCallId: String,
    content: Json,
    isError: Boolean = false
)

object ToolResult:
  given Encoder[ToolResult] = Encoder.instance: tr =>
    Json.obj(
      "tool_call_id" -> tr.toolCallId.asJson,
      "content" -> tr.content.asJson,
      "is_error" -> tr.isError.asJson
    )

  given Decoder[ToolResult] = Decoder.instance: c =>
    for
      id      <- c.downField("tool_call_id").as[String]
      content <- c.downField("content").as[Json]
      isError <- c.downField("is_error").as[Option[Boolean]]
    yield ToolResult(id, content, isError.getOrElse(false))

// ---------------------------------------------------------------------------
// ModelInfo
// ---------------------------------------------------------------------------

final case class ModelInfo(
    id: String,
    provider: String,
    displayName: String,
    contextWindow: Int,
    maxOutput: Int,
    supportsTools: Boolean = true,
    supportsVision: Boolean = true,
    supportsReasoning: Boolean = false,
    inputCostPerMillion: BigDecimal = BigDecimal(0),
    outputCostPerMillion: BigDecimal = BigDecimal(0),
    aliases: List[String] = Nil
)

object ModelInfo:
  given Encoder[ModelInfo] = Encoder.instance: m =>
    Json.obj(
      "id" -> m.id.asJson,
      "provider" -> m.provider.asJson,
      "display_name" -> m.displayName.asJson,
      "context_window" -> m.contextWindow.asJson,
      "max_output" -> m.maxOutput.asJson,
      "supports_tools" -> m.supportsTools.asJson,
      "supports_vision" -> m.supportsVision.asJson,
      "supports_reasoning" -> m.supportsReasoning.asJson,
      "input_cost_per_million" -> m.inputCostPerMillion.asJson,
      "output_cost_per_million" -> m.outputCostPerMillion.asJson,
      "aliases" -> m.aliases.asJson
    )

  given Decoder[ModelInfo] = Decoder.instance: c =>
    for
      id           <- c.downField("id").as[String]
      provider     <- c.downField("provider").as[String]
      displayName  <- c.downField("display_name").as[String]
      ctxWindow    <- c.downField("context_window").as[Int]
      maxOut       <- c.downField("max_output").as[Int]
      tools        <- c.downField("supports_tools").as[Option[Boolean]]
      vision       <- c.downField("supports_vision").as[Option[Boolean]]
      reasoning    <- c.downField("supports_reasoning").as[Option[Boolean]]
      inputCost    <- c.downField("input_cost_per_million").as[Option[BigDecimal]]
      outputCost   <- c.downField("output_cost_per_million").as[Option[BigDecimal]]
      aliases      <- c.downField("aliases").as[Option[List[String]]]
    yield ModelInfo(
      id, provider, displayName, ctxWindow, maxOut,
      tools.getOrElse(true), vision.getOrElse(true), reasoning.getOrElse(false),
      inputCost.getOrElse(BigDecimal(0)), outputCost.getOrElse(BigDecimal(0)),
      aliases.getOrElse(Nil)
    )
