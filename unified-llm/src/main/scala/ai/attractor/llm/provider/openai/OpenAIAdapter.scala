package ai.attractor.llm.provider.openai

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.syntax.all.*
import fs2.Stream
import io.circe.*
import io.circe.syntax.*
import org.http4s.{AuthScheme, Credentials, EntityDecoder, Headers, Header, MediaType, Method, Uri}
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

/** OpenAI Responses API adapter.
  *
  * Speaks the native OpenAI Responses API (`/v1/responses`) which properly
  * surfaces reasoning tokens for GPT-5.2 series models, supports built-in
  * tools, and server-side conversation state.
  */
final class OpenAIAdapter private (
  apiKey: String,
  baseUrl: String,
  httpClient: Http4sClient[IO]
) extends ProviderAdapter[IO]:

  val name: String = "openai"

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
    val httpReq = buildHttpRequest(body)
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
    val httpReq = buildHttpRequest(body)
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

  private def buildHttpRequest(body: Json): org.http4s.Request[IO] =
    val uri = Uri.unsafeFromString(s"$baseUrl/v1/responses")
    org.http4s.Request[IO](
      method = Method.POST,
      uri = uri,
      headers = Headers(
        Authorization(Credentials.Token(AuthScheme.Bearer, apiKey)),
        `Content-Type`(MediaType.application.json)
      )
    ).withEntity(body)

  // ---------------------------------------------------------------------------
  // Request body construction
  // ---------------------------------------------------------------------------

  private def buildRequestBody(request: Request, streaming: Boolean): Json =
    val (instructions, inputItems) = extractInstructions(request.messages)

    val base = Json.obj(
      "model" -> request.model.asJson,
      "input" -> inputItems.asJson
    )

    val withInstructions = instructions.fold(base): inst =>
      base.deepMerge(Json.obj("instructions" -> inst.asJson))

    val withStream = if streaming then
      withInstructions.deepMerge(Json.obj("stream" -> true.asJson))
    else withInstructions

    val withTools = if request.tools.nonEmpty then
      withStream.deepMerge(Json.obj("tools" -> translateTools(request.tools).asJson))
    else withStream

    val withToolChoice = request.toolChoice.fold(withTools): tc =>
      withTools.deepMerge(Json.obj("tool_choice" -> translateToolChoice(tc).asJson))

    val withTemp = request.temperature.fold(withToolChoice): t =>
      withToolChoice.deepMerge(Json.obj("temperature" -> t.asJson))

    val withTopP = request.topP.fold(withTemp): p =>
      withTemp.deepMerge(Json.obj("top_p" -> p.asJson))

    val withMaxTokens = request.maxTokens.fold(withTopP): mt =>
      withTopP.deepMerge(Json.obj("max_output_tokens" -> mt.asJson))

    // Reasoning effort for reasoning models (GPT-5.2 series)
    val withReasoning = request.reasoningEffort.fold(withMaxTokens): effort =>
      withMaxTokens.deepMerge(Json.obj(
        "reasoning" -> Json.obj("effort" -> effort.asJson)
      ))

    // Provider-specific options
    val withProviderOpts = request.providerOptions.fold(withReasoning): opts =>
      opts.hcursor.downField("openai").as[Json].toOption.fold(withReasoning): openaiOpts =>
        withReasoning.deepMerge(openaiOpts)

    withProviderOpts

  // ---------------------------------------------------------------------------
  // Message translation
  // ---------------------------------------------------------------------------

  /** Extract system messages as `instructions` and translate the rest to
    * Responses API `input` items.
    */
  private def extractInstructions(messages: List[Message]): (Option[String], List[Json]) =
    val systemTexts = messages
      .filter(m => m.role == Role.System || m.role == Role.Developer)
      .map(_.text)
      .filter(_.nonEmpty)
    val instructions = if systemTexts.nonEmpty then Some(systemTexts.mkString("\n\n")) else None
    val inputItems = messages
      .filterNot(m => m.role == Role.System || m.role == Role.Developer)
      .flatMap(translateInputItem)
    (instructions, inputItems)

  private def translateInputItem(msg: Message): List[Json] =
    msg.role match
      case Role.User =>
        List(Json.obj(
          "type" -> "message".asJson,
          "role" -> "user".asJson,
          "content" -> translateUserContent(msg.content).asJson
        ))

      case Role.Assistant =>
        val items = scala.collection.mutable.ListBuffer.empty[Json]
        // Emit text parts as message items, tool calls as function_call items
        val textParts = msg.content.filter(_.kind == ContentKind.Text)
        val toolParts = msg.content.filter(_.kind == ContentKind.ToolCall)

        if textParts.nonEmpty then
          items += Json.obj(
            "type" -> "message".asJson,
            "role" -> "assistant".asJson,
            "content" -> textParts.flatMap(cp => cp.text.map(_.asJson)).asJson
          )

        toolParts.foreach: cp =>
          cp.toolCall.foreach: tc =>
            val argsStr = tc.arguments.noSpaces
            items += Json.obj(
              "type" -> "function_call".asJson,
              "id" -> tc.id.asJson,
              "call_id" -> tc.id.asJson,
              "name" -> tc.name.asJson,
              "arguments" -> argsStr.asJson
            )

        items.toList

      case Role.Tool =>
        msg.content.collect:
          case cp if cp.kind == ContentKind.ToolResult =>
            cp.toolResult.map: tr =>
              val output = tr.content.asString.getOrElse(tr.content.noSpaces)
              Json.obj(
                "type" -> "function_call_output".asJson,
                "call_id" -> tr.toolCallId.asJson,
                "output" -> output.asJson
              )
        .flatten

      case _ => Nil

  private def translateUserContent(parts: List[ContentPart]): List[Json] =
    parts.flatMap: cp =>
      cp.kind match
        case ContentKind.Text =>
          cp.text.map: t =>
            Json.obj(
              "type" -> "input_text".asJson,
              "text" -> t.asJson
            )
          .toList
        case ContentKind.Image =>
          cp.image.map: img =>
            img.url match
              case Some(url) =>
                Json.obj(
                  "type" -> "input_image".asJson,
                  "image_url" -> url.asJson
                )
              case None =>
                val b64 = img.data.map(java.util.Base64.getEncoder.encodeToString).getOrElse("")
                val mt = img.mediaType.getOrElse("image/png")
                Json.obj(
                  "type" -> "input_image".asJson,
                  "image_url" -> s"data:$mt;base64,$b64".asJson
                )
          .toList
        case _ => Nil

  // ---------------------------------------------------------------------------
  // Tool definition translation
  // ---------------------------------------------------------------------------

  private def translateTools(tools: List[ToolDefinition]): List[Json] =
    tools.map: td =>
      Json.obj(
        "type" -> "function".asJson,
        "name" -> td.name.asJson,
        "description" -> td.description.asJson,
        "parameters" -> td.parameters
      )

  private def translateToolChoice(tc: ToolChoice): Json =
    tc.mode match
      case "auto"     => "auto".asJson
      case "none"     => "none".asJson
      case "required" => "required".asJson
      case "named"    =>
        tc.toolName.fold("auto".asJson): n =>
          Json.obj("type" -> "function".asJson, "name" -> n.asJson)
      case other => other.asJson

  // ---------------------------------------------------------------------------
  // Response parsing (non-streaming)
  // ---------------------------------------------------------------------------

  private def parseResponse(json: Json, request: Request): IO[Response] =
    val cursor = json.hcursor
    IO.fromEither(
      for
        id     <- cursor.downField("id").as[String]
        model  <- cursor.downField("model").as[Option[String]]
        output <- cursor.downField("output").as[List[Json]]
        status <- cursor.downField("status").as[Option[String]]
        usageJson <- cursor.downField("usage").as[Option[Json]]
      yield
        val parts = output.flatMap(parseOutputItem)
        val usage = usageJson.map(parseUsage).getOrElse(Usage.empty)
        val finish = mapFinishReason(status, parts)
        val message = Message(Role.Assistant, parts)
        Response(
          id = id,
          model = model.getOrElse(request.model),
          provider = name,
          message = message,
          finishReason = finish,
          usage = usage,
          raw = Some(json)
        )
    ).adaptError:
      case e: DecodingFailure =>
        InvalidRequestError(
          s"Failed to parse OpenAI response: ${e.getMessage}",
          provider = name,
          raw = Some(json)
        )

  private def parseOutputItem(item: Json): List[ContentPart] =
    val itemType = item.hcursor.downField("type").as[String].getOrElse("")
    itemType match
      case "message" =>
        val content = item.hcursor.downField("content").as[List[Json]].getOrElse(Nil)
        content.flatMap: c =>
          val cType = c.hcursor.downField("type").as[String].getOrElse("")
          cType match
            case "output_text" =>
              c.hcursor.downField("text").as[String].toOption.map(ContentPart.text).toList
            case _ => Nil
      case "function_call" =>
        val result = for
          id   <- item.hcursor.downField("id").as[String].orElse(
                     item.hcursor.downField("call_id").as[String])
          fnName <- item.hcursor.downField("name").as[String]
          argsStr <- item.hcursor.downField("arguments").as[String]
        yield
          val args = io.circe.parser.parse(argsStr).getOrElse(Json.fromString(argsStr))
          ContentPart.toolCall(ToolCallData(id, fnName, args))
        result.toOption.toList
      case _ => Nil

  private def parseUsage(json: Json): Usage =
    val c = json.hcursor
    val input = c.downField("input_tokens").as[Int].getOrElse(0)
    val output = c.downField("output_tokens").as[Int].getOrElse(0)
    val reasoning = c.downField("output_tokens_details")
      .downField("reasoning_tokens").as[Int].toOption
    val cacheRead = c.downField("input_tokens_details")
      .downField("cached_tokens").as[Int].toOption
    Usage(
      inputTokens = input,
      outputTokens = output,
      totalTokens = input + output,
      reasoningTokens = reasoning,
      cacheReadTokens = cacheRead,
      raw = Some(json)
    )

  private def mapFinishReason(status: Option[String], parts: List[ContentPart]): FinishReason =
    val hasToolCalls = parts.exists(_.kind == ContentKind.ToolCall)
    if hasToolCalls then FinishReason("tool_calls", status)
    else status match
      case Some("completed")      => FinishReason("stop", status)
      case Some("incomplete")     => FinishReason("length", status)
      case Some("failed")         => FinishReason("error", status)
      case Some("cancelled")      => FinishReason("other", status)
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
        IO.pure(translateOpenAIEvent(eventType, json))

  private def translateOpenAIEvent(eventType: String, json: Json): Option[StreamEvent] =
    eventType match
      case "response.created" =>
        val id = json.hcursor.downField("response").downField("id").as[String].toOption
        val model = json.hcursor.downField("response").downField("model").as[String].toOption
        Some(StreamEvent(
          eventType = StreamEventType.StreamStart,
          model = model,
          responseId = id,
          raw = Some(json)
        ))

      case "response.output_item.added" =>
        val itemType = json.hcursor.downField("item").downField("type").as[String].getOrElse("")
        itemType match
          case "function_call" =>
            val callId = json.hcursor.downField("item").downField("call_id").as[String].toOption
            val fnName = json.hcursor.downField("item").downField("name").as[String].toOption
            Some(StreamEvent(
              eventType = StreamEventType.ToolCallStart,
              toolCallId = callId,
              toolCallName = fnName,
              raw = Some(json)
            ))
          case "message" =>
            Some(StreamEvent(
              eventType = StreamEventType.ContentStart,
              raw = Some(json)
            ))
          case _ => None

      case "response.output_text.delta" =>
        val delta = json.hcursor.downField("delta").as[String].toOption
        Some(StreamEvent(
          eventType = StreamEventType.ContentDelta,
          contentDelta = delta,
          raw = Some(json)
        ))

      case "response.output_text.done" =>
        Some(StreamEvent(
          eventType = StreamEventType.ContentEnd,
          raw = Some(json)
        ))

      case "response.function_call_arguments.delta" =>
        val delta = json.hcursor.downField("delta").as[String].toOption
        Some(StreamEvent(
          eventType = StreamEventType.ToolCallDelta,
          argumentsDelta = delta,
          raw = Some(json)
        ))

      case "response.function_call_arguments.done" =>
        Some(StreamEvent(
          eventType = StreamEventType.ToolCallEnd,
          raw = Some(json)
        ))

      case "response.completed" =>
        val respJson = json.hcursor.downField("response").as[Json].toOption
        val usageJson = respJson.flatMap(_.hcursor.downField("usage").as[Json].toOption)
        val usage = usageJson.map(parseUsage)
        val status = respJson.flatMap(_.hcursor.downField("status").as[String].toOption)
        Some(StreamEvent(
          eventType = StreamEventType.StreamEnd,
          finishReason = Some(mapFinishReason(status, Nil)),
          usage = usage,
          raw = Some(json)
        ))

      case "error" =>
        val errMsg = json.hcursor.downField("message").as[String]
          .orElse(json.hcursor.downField("error").downField("message").as[String])
          .getOrElse("Unknown streaming error")
        Some(StreamEvent(
          eventType = StreamEventType.Error,
          error = Some(errMsg),
          raw = Some(json)
        ))

      case _ => None

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
      .orElse(body.hcursor.downField("message").as[String])
      .getOrElse(s"OpenAI API error (HTTP $statusCode)")

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
      case 408 =>
        RequestTimeoutError(errorMsg, provider = name, raw = Some(body))
      case s if s >= 500 =>
        ServerError(errorMsg, provider = name, statusCode = Some(s), raw = Some(body))
      case _ =>
        ProviderError(errorMsg, provider = name, statusCode = Some(statusCode), raw = Some(body))

object OpenAIAdapter:

  private val DefaultBaseUrl = "https://api.openai.com"

  def apply(
    apiKey: String,
    baseUrl: Option[String] = None,
    httpClient: Http4sClient[IO]
  ): OpenAIAdapter =
    new OpenAIAdapter(apiKey, baseUrl.getOrElse(DefaultBaseUrl), httpClient)

  /** Create an adapter as a cats-effect Resource that manages the HTTP client
    * lifecycle.
    */
  def resource(
    apiKey: String,
    baseUrl: Option[String] = None
  ): Resource[IO, OpenAIAdapter] =
    EmberClientBuilder.default[IO].build.map: client =>
      new OpenAIAdapter(apiKey, baseUrl.getOrElse(DefaultBaseUrl), client)
