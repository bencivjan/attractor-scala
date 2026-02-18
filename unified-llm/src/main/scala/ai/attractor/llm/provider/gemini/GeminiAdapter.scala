package ai.attractor.llm.provider.gemini

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.syntax.all.*
import fs2.Stream
import io.circe.*
import io.circe.syntax.*
import org.http4s.{EntityDecoder, Headers, Header, MediaType, Method, Uri}
import org.http4s.circe.*
import org.http4s.client.Client as Http4sClient
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.headers.*
import org.typelevel.ci.CIStringSyntax

import scala.concurrent.duration.*

import ai.attractor.llm.model.*
import ai.attractor.llm.model.{Request, Response, Message}
import ai.attractor.llm.provider.ProviderAdapter
import ai.attractor.llm.util.{SSEEvent, SSEParser}

/** Google Gemini API adapter.
  *
  * Speaks the native Gemini API (`/v1beta/models/{model}:generateContent`)
  * with support for system instructions, function calling, thinking, and
  * streaming via SSE.
  */
final class GeminiAdapter private (
  apiKey: String,
  baseUrl: String,
  httpClient: Http4sClient[IO]
) extends ProviderAdapter[IO]:

  val name: String = "gemini"

  // ---------------------------------------------------------------------------
  // Lifecycle
  // ---------------------------------------------------------------------------

  def close: IO[Unit] = IO.unit
  def initialize: IO[Unit] = IO.unit

  // ---------------------------------------------------------------------------
  // complete
  // ---------------------------------------------------------------------------

  def complete(request: Request): IO[Response] =
    val body = buildRequestBody(request)
    val uri = buildUri(request.model, streaming = false)
    val httpReq = org.http4s.Request[IO](
      method = Method.POST,
      uri = uri,
      headers = Headers(`Content-Type`(MediaType.application.json))
    ).withEntity(body)

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
    val body = buildRequestBody(request)
    val uri = buildUri(request.model, streaming = true)
    val httpReq = org.http4s.Request[IO](
      method = Method.POST,
      uri = uri,
      headers = Headers(`Content-Type`(MediaType.application.json))
    ).withEntity(body)

    Stream.resource(httpClient.run(httpReq)).flatMap: resp =>
      resp.status.code match
        case s if s >= 200 && s < 300 =>
          resp.body
            .through(SSEParser.parse)
            .filter(_.data != "[DONE]")
            .through(translateStreamChunks)
        case s =>
          Stream.eval(
            resp.as[Json].attempt.flatMap: bodyResult =>
              val rawJson = bodyResult.getOrElse(Json.Null)
              val retryAfter = parseRetryAfterHeader(resp.headers)
              IO.raiseError(mapHttpError(s, rawJson, retryAfter))
          )

  // ---------------------------------------------------------------------------
  // URI construction
  // ---------------------------------------------------------------------------

  private def buildUri(model: String, streaming: Boolean): Uri =
    val action = if streaming then "streamGenerateContent" else "generateContent"
    val base = s"$baseUrl/v1beta/models/$model:$action"
    val withKey = s"$base?key=$apiKey"
    val full = if streaming then s"$withKey&alt=sse" else withKey
    Uri.unsafeFromString(full)

  // ---------------------------------------------------------------------------
  // Request body construction
  // ---------------------------------------------------------------------------

  private def buildRequestBody(request: Request): Json =
    val (systemInstruction, contents) = buildContents(request.messages)

    val base = Json.obj("contents" -> contents.asJson)

    val withSystem = systemInstruction.fold(base): si =>
      base.deepMerge(Json.obj("systemInstruction" -> si))

    val withTools = if request.tools.nonEmpty then
      withSystem.deepMerge(Json.obj(
        "tools" -> Json.arr(
          Json.obj("functionDeclarations" -> translateTools(request.tools).asJson)
        )
      ))
    else withSystem

    val withToolConfig = request.toolChoice.fold(withTools): tc =>
      translateToolChoice(tc, request.tools) match
        case Some(config) =>
          withTools.deepMerge(Json.obj("toolConfig" -> config))
        case None => withTools

    val withGenConfig = buildGenerationConfig(request).fold(withToolConfig): gc =>
      withToolConfig.deepMerge(Json.obj("generationConfig" -> gc))

    // Provider-specific options from the "gemini" key
    val withProviderOpts = request.providerOptions.fold(withGenConfig): opts =>
      opts.hcursor.downField("gemini").as[Json].toOption.fold(withGenConfig): geminiOpts =>
        withGenConfig.deepMerge(geminiOpts)

    withProviderOpts

  // ---------------------------------------------------------------------------
  // Message translation
  // ---------------------------------------------------------------------------

  /** Build Gemini `contents` array and optional `systemInstruction`.
    *
    * System and developer messages are extracted to `systemInstruction`.
    * Tool results are folded into user turns. Assistant role maps to "model".
    */
  private def buildContents(messages: List[Message]): (Option[Json], List[Json]) =
    val systemMessages = messages.filter(m => m.role == Role.System || m.role == Role.Developer)
    val systemInstruction = if systemMessages.nonEmpty then
      val parts = systemMessages.flatMap(_.content).collect:
        case cp if cp.kind == ContentKind.Text =>
          Json.obj("text" -> cp.text.getOrElse("").asJson)
      Some(Json.obj("parts" -> parts.asJson))
    else None

    val conversationMessages = messages.filterNot(m => m.role == Role.System || m.role == Role.Developer)
    val contents = conversationMessages.map(translateMessage)
    (systemInstruction, contents)

  private def translateMessage(msg: Message): Json =
    val role = msg.role match
      case Role.User      => "user"
      case Role.Assistant  => "model"
      case Role.Tool       => "user"
      case _               => "user"

    val parts = msg.content.flatMap(translatePart(_, msg.role))
    Json.obj(
      "role" -> role.asJson,
      "parts" -> parts.asJson
    )

  private def translatePart(cp: ContentPart, role: Role): List[Json] =
    cp.kind match
      case ContentKind.Text =>
        cp.text.map(t => Json.obj("text" -> t.asJson)).toList

      case ContentKind.Image =>
        cp.image.map: img =>
          img.url match
            case Some(url) =>
              Json.obj("fileData" -> Json.obj(
                "fileUri" -> url.asJson,
                "mimeType" -> img.mediaType.getOrElse("image/png").asJson
              ))
            case None =>
              val b64 = img.data.map(java.util.Base64.getEncoder.encodeToString).getOrElse("")
              Json.obj("inlineData" -> Json.obj(
                "data" -> b64.asJson,
                "mimeType" -> img.mediaType.getOrElse("image/png").asJson
              ))
        .toList

      case ContentKind.ToolCall =>
        cp.toolCall.map: tc =>
          Json.obj("functionCall" -> Json.obj(
            "name" -> tc.name.asJson,
            "args" -> tc.arguments
          ))
        .toList

      case ContentKind.ToolResult =>
        cp.toolResult.map: tr =>
          val responseValue = tr.content.asString match
            case Some(s) =>
              io.circe.parser.parse(s).getOrElse(
                Json.obj("result" -> s.asJson)
              )
            case None =>
              if tr.content.isObject then tr.content
              else Json.obj("result" -> tr.content)
          Json.obj("functionResponse" -> Json.obj(
            "name" -> findToolNameForId(tr.toolCallId).asJson,
            "response" -> responseValue
          ))
        .toList

      case ContentKind.Thinking =>
        cp.thinking.map(td => Json.obj("thought" -> td.text.asJson)).toList

      case _ => Nil

  /** Best-effort lookup of tool name by call ID. Gemini does not use IDs
    * natively, so we store a name directly when available. Falls back to
    * the ID itself.
    */
  private def findToolNameForId(id: String): String =
    // The ID format is "call_{uuid}" which we generate on response parsing.
    // We cannot reverse-map it here, so the caller is responsible for
    // including the function name in the ToolResultData content or message.
    // As a fallback, return the ID stripped of "call_" prefix.
    if id.startsWith("call_") then id.stripPrefix("call_")
    else id

  // ---------------------------------------------------------------------------
  // Tool definition translation
  // ---------------------------------------------------------------------------

  private def translateTools(tools: List[ToolDefinition]): List[Json] =
    tools.map: td =>
      val base = Json.obj(
        "name" -> td.name.asJson,
        "description" -> td.description.asJson
      )
      // Gemini uses "parameters" for the JSON schema of the function args
      base.deepMerge(Json.obj("parameters" -> td.parameters))

  private def translateToolChoice(tc: ToolChoice, tools: List[ToolDefinition]): Option[Json] =
    tc.mode match
      case "auto" =>
        Some(Json.obj("functionCallingConfig" -> Json.obj(
          "mode" -> "AUTO".asJson
        )))
      case "none" =>
        Some(Json.obj("functionCallingConfig" -> Json.obj(
          "mode" -> "NONE".asJson
        )))
      case "required" =>
        Some(Json.obj("functionCallingConfig" -> Json.obj(
          "mode" -> "ANY".asJson
        )))
      case "named" =>
        tc.toolName.map: n =>
          Json.obj("functionCallingConfig" -> Json.obj(
            "mode" -> "ANY".asJson,
            "allowedFunctionNames" -> List(n).asJson
          ))
      case _ => None

  // ---------------------------------------------------------------------------
  // Generation config
  // ---------------------------------------------------------------------------

  private def buildGenerationConfig(request: Request): Option[Json] =
    val fields = List(
      request.temperature.map(t => "temperature" -> t.asJson),
      request.topP.map(p => "topP" -> p.asJson),
      request.maxTokens.map(m => "maxOutputTokens" -> m.asJson),
      Option.when(request.stopSequences.nonEmpty)(
        "stopSequences" -> request.stopSequences.asJson
      )
    ).flatten
    if fields.isEmpty then None
    else Some(Json.obj(fields*))

  // ---------------------------------------------------------------------------
  // Response parsing (non-streaming)
  // ---------------------------------------------------------------------------

  private def parseResponse(json: Json, request: Request): IO[Response] =
    val cursor = json.hcursor
    val candidates = cursor.downField("candidates").as[List[Json]].getOrElse(Nil)

    if candidates.isEmpty then
      // Check for prompt-level errors
      val blockReason = cursor.downField("promptFeedback")
        .downField("blockReason").as[String].toOption
      blockReason match
        case Some(reason) =>
          IO.raiseError(ContentFilterError(
            s"Gemini blocked the prompt: $reason",
            provider = name,
            raw = Some(json)
          ))
        case None =>
          IO.raiseError(ServerError(
            "Gemini returned no candidates",
            provider = name,
            raw = Some(json)
          ))
    else
      val candidate = candidates.head
      val contentParts = candidate.hcursor
        .downField("content").downField("parts").as[List[Json]].getOrElse(Nil)
      val parts = contentParts.flatMap(parseGeminiPart)
      val finishReason = candidate.hcursor.downField("finishReason").as[String].toOption
      val usageJson = cursor.downField("usageMetadata").as[Json].toOption
      val usage = usageJson.map(parseUsage).getOrElse(Usage.empty)
      val finish = mapFinishReason(finishReason, parts)

      val message = Message(Role.Assistant, parts)
      IO.pure(Response(
        id = generateResponseId(),
        model = request.model,
        provider = name,
        message = message,
        finishReason = finish,
        usage = usage,
        raw = Some(json)
      ))

  private def parseGeminiPart(part: Json): List[ContentPart] =
    val c = part.hcursor

    // Text part
    c.downField("text").as[String].toOption.map(ContentPart.text).toList ++
    // Function call part
    c.downField("functionCall").as[Json].toOption.flatMap: fc =>
      for
        fnName <- fc.hcursor.downField("name").as[String].toOption
        args = fc.hcursor.downField("args").as[Json].getOrElse(Json.obj())
      yield ContentPart.toolCall(ToolCallData(
        id = s"call_${java.util.UUID.randomUUID().toString.take(8)}",
        name = fnName,
        arguments = args
      ))
    .toList ++
    // Thought part (Gemini thinking)
    c.downField("thought").as[String].toOption.map: text =>
      ContentPart.thinking(ThinkingData(text))
    .toList

  private def parseUsage(json: Json): Usage =
    val c = json.hcursor
    val input = c.downField("promptTokenCount").as[Int].getOrElse(0)
    val output = c.downField("candidatesTokenCount").as[Int].getOrElse(0)
    val thoughts = c.downField("thoughtsTokenCount").as[Int].toOption
    val cached = c.downField("cachedContentTokenCount").as[Int].toOption
    Usage(
      inputTokens = input,
      outputTokens = output,
      totalTokens = input + output,
      reasoningTokens = thoughts,
      cacheReadTokens = cached,
      raw = Some(json)
    )

  private def mapFinishReason(raw: Option[String], parts: List[ContentPart]): FinishReason =
    val hasToolCalls = parts.exists(_.kind == ContentKind.ToolCall)
    if hasToolCalls then FinishReason("tool_calls", raw)
    else raw match
      case Some("STOP")        => FinishReason("stop", raw)
      case Some("MAX_TOKENS")  => FinishReason("length", raw)
      case Some("SAFETY")      => FinishReason("content_filter", raw)
      case Some("RECITATION")  => FinishReason("content_filter", raw)
      case Some(other)         => FinishReason("other", Some(other))
      case None                => FinishReason("stop", None)

  private def generateResponseId(): String =
    s"gemini-${java.util.UUID.randomUUID().toString.take(12)}"

  // ---------------------------------------------------------------------------
  // Streaming
  // ---------------------------------------------------------------------------

  /** Translate Gemini SSE chunks into unified StreamEvent values.
    *
    * Gemini streams JSON objects that have the same structure as the
    * non-streaming response (candidates/content/parts + usageMetadata).
    * Each SSE event is a partial response chunk.
    */
  private def translateStreamChunks: fs2.Pipe[IO, SSEEvent, StreamEvent] =
    (events: Stream[IO, SSEEvent]) =>
      var started = false
      events.flatMap: sse =>
        io.circe.parser.parse(sse.data) match
          case Left(_) => Stream.empty
          case Right(json) =>
            val streamStartEvt =
              if !started then
                started = true
                Stream.emit(StreamEvent(
                  eventType = StreamEventType.StreamStart,
                  raw = Some(json)
                ))
              else Stream.empty

            val candidates = json.hcursor.downField("candidates").as[List[Json]].getOrElse(Nil)
            val contentEvents = candidates.headOption.toList.flatMap: candidate =>
              val parts = candidate.hcursor
                .downField("content").downField("parts").as[List[Json]].getOrElse(Nil)
              parts.flatMap(translateStreamPart)

            val usageJson = json.hcursor.downField("usageMetadata").as[Json].toOption
            val usageEvents = usageJson.map: uj =>
              StreamEvent(
                eventType = StreamEventType.UsageUpdate,
                usage = Some(parseUsage(uj)),
                raw = Some(json)
              )
            .toList

            // Check if this is the final chunk (has finishReason)
            val finishReason = candidates.headOption
              .flatMap(_.hcursor.downField("finishReason").as[String].toOption)
            val finishEvents = finishReason.map: _ =>
              val parts = candidates.headOption.toList.flatMap: candidate =>
                candidate.hcursor.downField("content").downField("parts")
                  .as[List[Json]].getOrElse(Nil).flatMap(parseGeminiPart)
              StreamEvent(
                eventType = StreamEventType.StreamEnd,
                finishReason = Some(mapFinishReason(finishReason, parts)),
                usage = usageJson.map(parseUsage),
                raw = Some(json)
              )
            .toList

            streamStartEvt ++ Stream.emits(contentEvents ++ usageEvents ++ finishEvents)

  private def translateStreamPart(part: Json): List[StreamEvent] =
    val c = part.hcursor
    // Text delta
    c.downField("text").as[String].toOption.map: text =>
      StreamEvent(
        eventType = StreamEventType.ContentDelta,
        contentDelta = Some(text),
        raw = Some(part)
      )
    .toList ++
    // Function call
    c.downField("functionCall").as[Json].toOption.flatMap: fc =>
      for
        fnName <- fc.hcursor.downField("name").as[String].toOption
      yield
        val args = fc.hcursor.downField("args").as[Json].getOrElse(Json.obj())
        val callId = s"call_${java.util.UUID.randomUUID().toString.take(8)}"
        StreamEvent(
          eventType = StreamEventType.ToolCallStart,
          toolCallId = Some(callId),
          toolCallName = Some(fnName),
          argumentsDelta = Some(args.noSpaces),
          raw = Some(part)
        )
    .toList ++
    // Thought delta
    c.downField("thought").as[String].toOption.map: text =>
      StreamEvent(
        eventType = StreamEventType.ThinkingDelta,
        thinkingDelta = Some(text),
        raw = Some(part)
      )
    .toList

  // ---------------------------------------------------------------------------
  // Error mapping
  // ---------------------------------------------------------------------------

  private def parseRetryAfterHeader(headers: Headers): Option[FiniteDuration] =
    headers.get(ci"retry-after").flatMap: nel =>
      val value = nel.head.value.trim
      value.toLongOption.map(_.seconds).orElse:
        scala.util.Try:
          import java.time.format.DateTimeFormatter
          import java.time.{ZonedDateTime, Instant}
          val date = ZonedDateTime.parse(value, DateTimeFormatter.RFC_1123_DATE_TIME)
          val delayMs = java.time.Duration.between(Instant.now(), date.toInstant).toMillis
          if delayMs > 0 then delayMs.millis else 0.millis
        .toOption

  private def mapHttpError(statusCode: Int, body: Json, retryAfter: Option[FiniteDuration] = None): ProviderError =
    val errorMsg = body.hcursor.downField("error").downField("message").as[String]
      .getOrElse(s"Gemini API error (HTTP $statusCode)")
    val errorStatus = body.hcursor.downField("error").downField("status").as[String].toOption

    statusCode match
      case 401 =>
        AuthenticationError(errorMsg, provider = name, raw = Some(body))
      case 403 =>
        AccessDeniedError(errorMsg, provider = name, raw = Some(body))
      case 404 =>
        NotFoundError(errorMsg, provider = name, raw = Some(body))
      case 400 =>
        InvalidRequestError(errorMsg, provider = name, raw = Some(body))
      case 429 =>
        RateLimitError(errorMsg, provider = name, retryAfter = retryAfter, raw = Some(body))
      case s if s >= 500 =>
        ServerError(errorMsg, provider = name, statusCode = Some(s), raw = Some(body))
      case _ =>
        ProviderError(errorMsg, provider = name, statusCode = Some(statusCode), raw = Some(body))

object GeminiAdapter:

  private val DefaultBaseUrl = "https://generativelanguage.googleapis.com"

  def apply(
    apiKey: String,
    baseUrl: Option[String] = None,
    httpClient: Http4sClient[IO]
  ): GeminiAdapter =
    new GeminiAdapter(apiKey, baseUrl.getOrElse(DefaultBaseUrl), httpClient)

  /** Create an adapter as a cats-effect Resource that manages the HTTP client
    * lifecycle.
    */
  def resource(
    apiKey: String,
    baseUrl: Option[String] = None
  ): Resource[IO, GeminiAdapter] =
    EmberClientBuilder.default[IO].build.map: client =>
      new GeminiAdapter(apiKey, baseUrl.getOrElse(DefaultBaseUrl), client)
