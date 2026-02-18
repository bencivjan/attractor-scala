package ai.attractor.llm.provider.anthropic

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.syntax.all.*
import fs2.Stream
import io.circe.*
import io.circe.syntax.*
import org.http4s.{EntityDecoder, Headers, Header, MediaType, Method, Uri}
import org.typelevel.ci.CIStringSyntax
import org.http4s.circe.*
import org.http4s.client.Client as Http4sClient
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.headers.*

import scala.concurrent.duration.*

import ai.attractor.llm.model.*
import ai.attractor.llm.model.{Request, Response, Message}
import ai.attractor.llm.provider.ProviderAdapter
import ai.attractor.llm.util.{SSEEvent, SSEParser}

/** Anthropic Messages API adapter.
  *
  * Speaks the native Anthropic Messages API (`/v1/messages`) with support for
  * extended thinking, prompt caching, tool use, and streaming.
  */
final class AnthropicAdapter private (
  apiKey: String,
  baseUrl: String,
  httpClient: Http4sClient[IO]
) extends ProviderAdapter[IO]:

  val name: String = "anthropic"

  private val DefaultMaxTokens = 16384
  private val ApiVersion       = "2023-06-01"

  // ---------------------------------------------------------------------------
  // Lifecycle
  // ---------------------------------------------------------------------------

  def close: IO[Unit] = IO.unit
  def initialize: IO[Unit] = IO.unit

  // ---------------------------------------------------------------------------
  // complete
  // ---------------------------------------------------------------------------

  def complete(request: Request): IO[Response] =
    val body = buildRequestBody(request, streaming = false)
    val httpReq = buildHttpRequest(request, body)
    httpClient.run(httpReq).use: resp =>
      resp.status.code match
        case s if s >= 200 && s < 300 =>
          resp.as[Json].flatMap(parseResponse(_, request))
        case s =>
          resp.as[Json].attempt.flatMap: bodyResult =>
            val rawJson = bodyResult.getOrElse(Json.Null)
            val retryAfter = parseRetryAfterHeader(resp.headers)
            IO.raiseError(mapHttpError(s, rawJson, retryAfter))

  // ---------------------------------------------------------------------------
  // stream
  // ---------------------------------------------------------------------------

  def stream(request: Request): Stream[IO, StreamEvent] =
    val body = buildRequestBody(request, streaming = true)
    val httpReq = buildHttpRequest(request, body)
    Stream.resource(httpClient.run(httpReq)).flatMap: resp =>
      resp.status.code match
        case s if s >= 200 && s < 300 =>
          resp.body
            .through(SSEParser.parse)
            .filter(_.data != "[DONE]")
            .evalMap(translateSSEEvent)
            .unNone
        case s =>
          Stream.eval(
            resp.as[Json].attempt.flatMap: bodyResult =>
              val rawJson = bodyResult.getOrElse(Json.Null)
              val retryAfter = parseRetryAfterHeader(resp.headers)
              IO.raiseError(mapHttpError(s, rawJson, retryAfter))
          )

  // ---------------------------------------------------------------------------
  // HTTP request construction
  // ---------------------------------------------------------------------------

  private def buildHttpRequest(request: Request, body: Json): org.http4s.Request[IO] =
    val uri = Uri.unsafeFromString(s"$baseUrl/v1/messages")
    val betaHeaders = extractBetaHeaders(request)
    val baseHeaders = Headers(
      Header.Raw(ci"x-api-key", apiKey),
      Header.Raw(ci"anthropic-version", ApiVersion),
      `Content-Type`(MediaType.application.json)
    )
    val allHeaders =
      if betaHeaders.nonEmpty then
        baseHeaders.put(Header.Raw(ci"anthropic-beta", betaHeaders.mkString(",")))
      else baseHeaders

    org.http4s.Request[IO](
      method = Method.POST,
      uri = uri,
      headers = allHeaders
    ).withEntity(body)

  private def extractBetaHeaders(request: Request): List[String] =
    for
      opts <- request.providerOptions.toList
      anthropic <- opts.hcursor.downField("anthropic").as[Json].toOption.toList
      headers <- anthropic.hcursor.downField("beta_headers").as[List[String]].toOption.toList
      header <- headers
    yield header

  // ---------------------------------------------------------------------------
  // Request body construction
  // ---------------------------------------------------------------------------

  private def buildRequestBody(request: Request, streaming: Boolean): Json =
    val autoCache = isAutoCacheEnabled(request)
    val (systemParts, conversationMessages) = extractSystemMessages(request.messages)
    val anthropicMessages = translateMessages(conversationMessages, autoCache)
    val maxTokens = request.maxTokens.getOrElse(DefaultMaxTokens)

    val base = Json.obj(
      "model" -> request.model.asJson,
      "max_tokens" -> maxTokens.asJson,
      "messages" -> anthropicMessages.asJson
    )

    val withSystem = if systemParts.nonEmpty then
      base.deepMerge(Json.obj("system" -> buildSystemParam(systemParts, autoCache)))
    else base

    val withStream = if streaming then
      withSystem.deepMerge(Json.obj("stream" -> true.asJson))
    else withSystem

    val withTools = if request.tools.nonEmpty then
      withStream.deepMerge(Json.obj("tools" -> translateTools(request.tools).asJson))
    else withStream

    val withToolChoice = request.toolChoice.fold(withTools): tc =>
      translateToolChoice(tc) match
        case Some(tcJson) => withTools.deepMerge(Json.obj("tool_choice" -> tcJson))
        case None         => withTools

    val withTemp = request.temperature.fold(withToolChoice): t =>
      withToolChoice.deepMerge(Json.obj("temperature" -> t.asJson))

    val withTopP = request.topP.fold(withTemp): p =>
      withTemp.deepMerge(Json.obj("top_p" -> p.asJson))

    val withStop = if request.stopSequences.nonEmpty then
      withTopP.deepMerge(Json.obj("stop_sequences" -> request.stopSequences.asJson))
    else withTopP

    // Inject provider-specific options from the "anthropic" key
    val withProviderOpts = request.providerOptions.fold(withStop): opts =>
      opts.hcursor.downField("anthropic").as[Json].toOption.fold(withStop): anthropicOpts =>
        // Pass through thinking config if present
        anthropicOpts.hcursor.downField("thinking").as[Json].toOption.fold(withStop): thinking =>
          withStop.deepMerge(Json.obj("thinking" -> thinking))

    withProviderOpts

  // ---------------------------------------------------------------------------
  // System message extraction and cache control injection
  // ---------------------------------------------------------------------------

  /** Extract system-role and developer-role messages, returning them separately
    * from the conversation messages. System messages are extracted to the
    * `system` parameter per the Anthropic API contract.
    */
  /** Check if automatic prompt caching is enabled (default: true).
    * Can be disabled via `provider_options.anthropic.auto_cache = false`.
    */
  private def isAutoCacheEnabled(request: Request): Boolean =
    request.providerOptions.flatMap: opts =>
      opts.hcursor.downField("anthropic").downField("auto_cache").as[Boolean].toOption
    .getOrElse(true)

  private def extractSystemMessages(messages: List[Message]): (List[Message], List[Message]) =
    messages.partition(m => m.role == Role.System || m.role == Role.Developer)

  /** Build the Anthropic `system` parameter from extracted system messages.
    * Injects cache_control on the last system block for prompt caching.
    */
  private def buildSystemParam(systemMessages: List[Message], autoCache: Boolean): Json =
    val blocks = systemMessages.flatMap: msg =>
      msg.content.collect:
        case cp if cp.kind == ContentKind.Text =>
          Json.obj("type" -> "text".asJson, "text" -> cp.text.getOrElse("").asJson)

    if blocks.isEmpty then Json.Null
    else if autoCache then
      // Inject cache_control on the last system block
      val withCache = blocks.init :+ blocks.last.deepMerge(
        Json.obj("cache_control" -> Json.obj("type" -> "ephemeral".asJson))
      )
      withCache.asJson
    else
      blocks.asJson

  // ---------------------------------------------------------------------------
  // Message translation
  // ---------------------------------------------------------------------------

  /** Translate unified messages to Anthropic format.
    *
    * Enforces strict user/assistant alternation by merging consecutive
    * same-role messages. Tool-result messages are folded into user messages.
    */
  private def translateMessages(messages: List[Message], autoCache: Boolean): List[Json] =
    val mapped = messages.flatMap(translateSingleMessage)
    val merged = mergeConsecutiveSameRole(mapped)
    if autoCache then injectUserCacheControl(merged)
    else merged

  private def translateSingleMessage(msg: Message): List[Json] =
    msg.role match
      case Role.User =>
        List(Json.obj(
          "role" -> "user".asJson,
          "content" -> translateContentParts(msg.content, Role.User).asJson
        ))
      case Role.Assistant =>
        List(Json.obj(
          "role" -> "assistant".asJson,
          "content" -> translateContentParts(msg.content, Role.Assistant).asJson
        ))
      case Role.Tool =>
        // Tool results are sent as user messages with tool_result content blocks
        val toolResultBlocks = msg.content.collect:
          case cp if cp.kind == ContentKind.ToolResult =>
            cp.toolResult.map(translateToolResult).getOrElse(Json.Null)
        .filter(_ != Json.Null)
        if toolResultBlocks.nonEmpty then
          List(Json.obj(
            "role" -> "user".asJson,
            "content" -> toolResultBlocks.asJson
          ))
        else Nil
      case Role.System | Role.Developer =>
        // Already extracted; should not appear here
        Nil

  private def translateContentParts(parts: List[ContentPart], role: Role): List[Json] =
    parts.flatMap: cp =>
      cp.kind match
        case ContentKind.Text =>
          List(Json.obj(
            "type" -> "text".asJson,
            "text" -> cp.text.getOrElse("").asJson
          ))
        case ContentKind.Image =>
          cp.image.map(translateImage).toList
        case ContentKind.ToolCall =>
          cp.toolCall.map(translateToolCall).toList
        case ContentKind.ToolResult =>
          cp.toolResult.map(translateToolResult).toList
        case ContentKind.Thinking =>
          cp.thinking.map: td =>
            val base = Json.obj(
              "type" -> "thinking".asJson,
              "thinking" -> td.text.asJson
            )
            td.signature.fold(base)(sig =>
              base.deepMerge(Json.obj("signature" -> sig.asJson))
            )
          .toList
        case ContentKind.RedactedThinking =>
          cp.thinking.map: td =>
            Json.obj("type" -> "redacted_thinking".asJson, "data" -> td.text.asJson)
          .toList
        case _ => Nil

  private def translateImage(img: ImageData): Json =
    img.url match
      case Some(url) =>
        Json.obj(
          "type" -> "image".asJson,
          "source" -> Json.obj(
            "type" -> "url".asJson,
            "url" -> url.asJson
          )
        )
      case None =>
        val b64 = img.data.map(java.util.Base64.getEncoder.encodeToString).getOrElse("")
        val mt = img.mediaType.getOrElse("image/png")
        Json.obj(
          "type" -> "image".asJson,
          "source" -> Json.obj(
            "type" -> "base64".asJson,
            "media_type" -> mt.asJson,
            "data" -> b64.asJson
          )
        )

  private def translateToolCall(tc: ToolCallData): Json =
    Json.obj(
      "type" -> "tool_use".asJson,
      "id" -> tc.id.asJson,
      "name" -> tc.name.asJson,
      "input" -> tc.arguments
    )

  private def translateToolResult(tr: ToolResultData): Json =
    val contentValue = tr.content.asString match
      case Some(s) => s.asJson
      case None    => tr.content
    val base = Json.obj(
      "type" -> "tool_result".asJson,
      "tool_use_id" -> tr.toolCallId.asJson,
      "content" -> contentValue
    )
    if tr.isError then base.deepMerge(Json.obj("is_error" -> true.asJson))
    else base

  // ---------------------------------------------------------------------------
  // Tool definition translation
  // ---------------------------------------------------------------------------

  private def translateTools(tools: List[ToolDefinition]): List[Json] =
    tools.map: td =>
      Json.obj(
        "name" -> td.name.asJson,
        "description" -> td.description.asJson,
        "input_schema" -> td.parameters
      )

  private def translateToolChoice(tc: ToolChoice): Option[Json] =
    tc.mode match
      case "auto"     => Some(Json.obj("type" -> "auto".asJson))
      case "required" => Some(Json.obj("type" -> "any".asJson))
      case "named"    =>
        tc.toolName.map(n => Json.obj("type" -> "tool".asJson, "name" -> n.asJson))
      case "none"     => None // Omit tools entirely
      case _          => Some(Json.obj("type" -> "auto".asJson))

  // ---------------------------------------------------------------------------
  // Consecutive-role merging for strict alternation
  // ---------------------------------------------------------------------------

  private case class RoleContent(role: String, content: List[Json])

  private def mergeConsecutiveSameRole(messages: List[Json]): List[Json] =
    if messages.isEmpty then return Nil
    val grouped = messages.foldLeft(List.empty[RoleContent]): (acc, msg) =>
      val role = msg.hcursor.downField("role").as[String].getOrElse("")
      val content = msg.hcursor.downField("content").as[List[Json]].getOrElse(Nil)
      acc match
        case head :: tail if head.role == role =>
          head.copy(content = head.content ++ content) :: tail
        case _ =>
          RoleContent(role, content) :: acc
    grouped.reverse.map: rc =>
      Json.obj(
        "role" -> rc.role.asJson,
        "content" -> rc.content.asJson
      )

  /** Inject cache_control on the content of the last user message. */
  private def injectUserCacheControl(messages: List[Json]): List[Json] =
    val lastUserIdx = messages.lastIndexWhere: msg =>
      msg.hcursor.downField("role").as[String].toOption.contains("user")
    if lastUserIdx < 0 then messages
    else
      val msg = messages(lastUserIdx)
      val contentArr = msg.hcursor.downField("content").as[List[Json]].getOrElse(Nil)
      if contentArr.isEmpty then messages
      else
        val lastBlock = contentArr.last.deepMerge(
          Json.obj("cache_control" -> Json.obj("type" -> "ephemeral".asJson))
        )
        val newContent = contentArr.init :+ lastBlock
        val newMsg = Json.obj(
          "role" -> "user".asJson,
          "content" -> newContent.asJson
        )
        messages.updated(lastUserIdx, newMsg)

  // ---------------------------------------------------------------------------
  // Response parsing (non-streaming)
  // ---------------------------------------------------------------------------

  private def parseResponse(json: Json, request: Request): IO[Response] =
    val cursor = json.hcursor
    IO.fromEither(
      for
        id    <- cursor.downField("id").as[String]
        model <- cursor.downField("model").as[String]
        contentBlocks <- cursor.downField("content").as[List[Json]]
        stopReason <- cursor.downField("stop_reason").as[Option[String]]
        usageJson  <- cursor.downField("usage").as[Json]
      yield
        val parts = contentBlocks.flatMap(parseContentBlock)
        val usage = parseUsage(usageJson)
        val finish = mapFinishReason(stopReason)
        val message = Message(Role.Assistant, parts)
        Response(
          id = id,
          model = model,
          provider = name,
          message = message,
          finishReason = finish,
          usage = usage,
          raw = Some(json)
        )
    ).adaptError:
      case e: DecodingFailure =>
        InvalidRequestError(
          s"Failed to parse Anthropic response: ${e.getMessage}",
          provider = name,
          raw = Some(json)
        )

  private def parseContentBlock(block: Json): Option[ContentPart] =
    block.hcursor.downField("type").as[String].toOption.flatMap:
      case "text" =>
        block.hcursor.downField("text").as[String].toOption.map(ContentPart.text)
      case "tool_use" =>
        for
          id   <- block.hcursor.downField("id").as[String].toOption
          name <- block.hcursor.downField("name").as[String].toOption
          input = block.hcursor.downField("input").as[Json].getOrElse(Json.obj())
        yield ContentPart.toolCall(ToolCallData(id, name, input))
      case "thinking" =>
        for
          text <- block.hcursor.downField("thinking").as[String].toOption
          sig = block.hcursor.downField("signature").as[String].toOption
        yield ContentPart.thinking(ThinkingData(text, sig))
      case "redacted_thinking" =>
        val data = block.hcursor.downField("data").as[String].getOrElse("")
        Some(ContentPart.redactedThinking(ThinkingData(data, redacted = true)))
      case _ => None

  private def parseUsage(json: Json): Usage =
    val c = json.hcursor
    Usage(
      inputTokens = c.downField("input_tokens").as[Int].getOrElse(0),
      outputTokens = c.downField("output_tokens").as[Int].getOrElse(0),
      totalTokens =
        c.downField("input_tokens").as[Int].getOrElse(0) +
        c.downField("output_tokens").as[Int].getOrElse(0),
      cacheReadTokens = c.downField("cache_read_input_tokens").as[Int].toOption,
      cacheWriteTokens = c.downField("cache_creation_input_tokens").as[Int].toOption,
      raw = Some(json)
    )

  private def mapFinishReason(raw: Option[String]): FinishReason =
    raw match
      case Some("end_turn")       => FinishReason("stop", raw)
      case Some("stop_sequence")  => FinishReason("stop", raw)
      case Some("max_tokens")     => FinishReason("length", raw)
      case Some("tool_use")       => FinishReason("tool_calls", raw)
      case Some(other)            => FinishReason("other", Some(other))
      case None                   => FinishReason("stop", None)

  // ---------------------------------------------------------------------------
  // SSE event translation (streaming)
  // ---------------------------------------------------------------------------

  private def translateSSEEvent(sse: SSEEvent): IO[Option[StreamEvent]] =
    io.circe.parser.parse(sse.data) match
      case Left(_) => IO.pure(None)
      case Right(json) =>
        val eventType = sse.eventType
          .orElse(json.hcursor.downField("type").as[String].toOption)
          .getOrElse("")
        IO.pure(translateAnthropicEvent(eventType, json))

  private def translateAnthropicEvent(eventType: String, json: Json): Option[StreamEvent] =
    eventType match
      case "message_start" =>
        val model = json.hcursor.downField("message").downField("model").as[String].toOption
        val id = json.hcursor.downField("message").downField("id").as[String].toOption
        Some(StreamEvent(
          eventType = StreamEventType.StreamStart,
          model = model,
          responseId = id,
          raw = Some(json)
        ))

      case "content_block_start" =>
        val blockType = json.hcursor
          .downField("content_block").downField("type").as[String].getOrElse("")
        blockType match
          case "text" =>
            Some(StreamEvent(
              eventType = StreamEventType.ContentStart,
              raw = Some(json)
            ))
          case "tool_use" =>
            val toolId = json.hcursor.downField("content_block").downField("id").as[String].toOption
            val toolName = json.hcursor.downField("content_block").downField("name").as[String].toOption
            Some(StreamEvent(
              eventType = StreamEventType.ToolCallStart,
              toolCallId = toolId,
              toolCallName = toolName,
              raw = Some(json)
            ))
          case "thinking" =>
            Some(StreamEvent(
              eventType = StreamEventType.ThinkingStart,
              raw = Some(json)
            ))
          case _ => None

      case "content_block_delta" =>
        val deltaType = json.hcursor
          .downField("delta").downField("type").as[String].getOrElse("")
        deltaType match
          case "text_delta" =>
            val text = json.hcursor.downField("delta").downField("text").as[String].toOption
            Some(StreamEvent(
              eventType = StreamEventType.ContentDelta,
              contentDelta = text,
              raw = Some(json)
            ))
          case "input_json_delta" =>
            val partial = json.hcursor.downField("delta").downField("partial_json").as[String].toOption
            Some(StreamEvent(
              eventType = StreamEventType.ToolCallDelta,
              argumentsDelta = partial,
              raw = Some(json)
            ))
          case "thinking_delta" =>
            val thinking = json.hcursor.downField("delta").downField("thinking").as[String].toOption
            Some(StreamEvent(
              eventType = StreamEventType.ThinkingDelta,
              thinkingDelta = thinking,
              raw = Some(json)
            ))
          case "signature_delta" =>
            val sig = json.hcursor.downField("delta").downField("signature").as[String].toOption
            Some(StreamEvent(
              eventType = StreamEventType.ThinkingDelta,
              thinkingSignature = sig,
              raw = Some(json)
            ))
          case _ => None

      case "content_block_stop" =>
        val idx = json.hcursor.downField("index").as[Int].toOption
        // We do not know the block type from the stop event alone.
        // Emit ContentEnd as a safe default; consumers track state.
        Some(StreamEvent(
          eventType = StreamEventType.ContentEnd,
          raw = Some(json)
        ))

      case "message_delta" =>
        val stopReason = json.hcursor.downField("delta").downField("stop_reason").as[String].toOption
        val usageJson = json.hcursor.downField("usage").as[Json].toOption
        val usage = usageJson.map(parseUsage)
        val finish = stopReason.map(r => mapFinishReason(Some(r)))
        Some(StreamEvent(
          eventType = StreamEventType.UsageUpdate,
          finishReason = finish,
          usage = usage,
          raw = Some(json)
        ))

      case "message_stop" =>
        Some(StreamEvent(
          eventType = StreamEventType.StreamEnd,
          raw = Some(json)
        ))

      case "error" =>
        val errMsg = json.hcursor.downField("error").downField("message").as[String].getOrElse("Unknown error")
        Some(StreamEvent(
          eventType = StreamEventType.Error,
          error = Some(errMsg),
          raw = Some(json)
        ))

      case "ping" => None

      case _ => None

  // ---------------------------------------------------------------------------
  // Error mapping
  // ---------------------------------------------------------------------------

  private def parseRetryAfterHeader(headers: Headers): Option[FiniteDuration] =
    headers.get(ci"retry-after").flatMap: nel =>
      val value = nel.head.value.trim
      value.toLongOption.map(_.seconds).orElse:
        // Try parsing HTTP-date format
        scala.util.Try:
          import java.time.format.DateTimeFormatter
          import java.time.{ZonedDateTime, Instant}
          val date = ZonedDateTime.parse(value, DateTimeFormatter.RFC_1123_DATE_TIME)
          val delayMs = java.time.Duration.between(Instant.now(), date.toInstant).toMillis
          if delayMs > 0 then delayMs.millis else 0.millis
        .toOption

  private def mapHttpError(statusCode: Int, body: Json, retryAfter: Option[FiniteDuration] = None): ProviderError =
    val errorMsg = body.hcursor
      .downField("error").downField("message").as[String]
      .getOrElse(s"Anthropic API error (HTTP $statusCode)")
    val errorType = body.hcursor
      .downField("error").downField("type").as[String].toOption

    statusCode match
      case 401 =>
        AuthenticationError(errorMsg, provider = name, raw = Some(body))
      case 403 =>
        AccessDeniedError(errorMsg, provider = name, raw = Some(body))
      case 404 =>
        NotFoundError(errorMsg, provider = name, raw = Some(body))
      case 400 =>
        // Check for context length errors
        if errorType.contains("invalid_request_error") &&
           errorMsg.toLowerCase.contains("token") then
          ContextLengthError(errorMsg, provider = name, raw = Some(body))
        else
          InvalidRequestError(errorMsg, provider = name, raw = Some(body))
      case 429 =>
        RateLimitError(errorMsg, provider = name, retryAfter = retryAfter, raw = Some(body))
      case 408 =>
        RequestTimeoutError(errorMsg, provider = name, raw = Some(body))
      case 529 =>
        ServerError(errorMsg, provider = name, statusCode = Some(529), raw = Some(body))
      case s if s >= 500 =>
        ServerError(errorMsg, provider = name, statusCode = Some(s), raw = Some(body))
      case _ =>
        ProviderError(errorMsg, provider = name, statusCode = Some(statusCode), raw = Some(body))

object AnthropicAdapter:

  private val DefaultBaseUrl = "https://api.anthropic.com"

  def apply(
    apiKey: String,
    baseUrl: Option[String] = None,
    httpClient: Http4sClient[IO]
  ): AnthropicAdapter =
    new AnthropicAdapter(apiKey, baseUrl.getOrElse(DefaultBaseUrl), httpClient)

  /** Create an adapter as a cats-effect Resource that manages the HTTP client
    * lifecycle.
    */
  def resource(
    apiKey: String,
    baseUrl: Option[String] = None
  ): Resource[IO, AnthropicAdapter] =
    EmberClientBuilder.default[IO].build.map: client =>
      new AnthropicAdapter(apiKey, baseUrl.getOrElse(DefaultBaseUrl), client)
