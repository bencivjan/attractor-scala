package ai.attractor.llm.api

import ai.attractor.llm.model.*
import ai.attractor.llm.provider.Client
import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all.*
import cats.effect.syntax.all.*
import fs2.Stream
import io.circe.Json

/** Result of a streaming generate call.
  *
  * @param events     the raw event stream including tool-call rounds
  * @param response   a deferred that completes with the final accumulated Response
  *                   once the stream is fully consumed
  * @param textStream convenience projection that yields only text content deltas
  */
final case class StreamResult(
    events: Stream[IO, StreamEvent],
    response: IO[Response],
    textStream: Stream[IO, String]
)

/** High-level streaming API that mirrors [[Generate]] but returns an
  * incrementally consumable stream rather than blocking until completion.
  *
  * When tools with execute handlers are provided and the model issues tool
  * calls, the stream transparently pauses after the first model turn,
  * executes the tools, feeds results back, and resumes streaming the next
  * model turn -- up to `maxToolRounds` iterations.
  */
object StreamApi:

  def apply(
      model: String,
      prompt: Option[String] = None,
      messages: Option[List[Message]] = None,
      system: Option[String] = None,
      tools: Option[List[Tool]] = None,
      toolChoice: Option[ToolChoice] = None,
      maxToolRounds: Int = 1,
      responseFormat: Option[ResponseFormat] = None,
      temperature: Option[Double] = None,
      topP: Option[Double] = None,
      maxTokens: Option[Int] = None,
      stopSequences: Option[List[String]] = None,
      reasoningEffort: Option[String] = None,
      provider: Option[String] = None,
      providerOptions: Option[Json] = None,
      client: Option[Client] = None
  ): IO[StreamResult] =
    for
      _         <- validateInputs(prompt, messages)
      resolved  <- resolveClient(client)
      deferred  <- Deferred[IO, Response]
      initial    = buildInitialMessages(system, prompt, messages)
      toolDefs   = tools.map(_.map(_.toDefinition)).getOrElse(Nil)
      toolIndex  = tools.map(_.flatMap(t => t.execute.map(t.name -> _)).toMap).getOrElse(Map.empty)
    yield
      val events = buildStream(
        resolved, model, initial, toolDefs, toolChoice, toolIndex,
        maxToolRounds, responseFormat, temperature, topP, maxTokens,
        stopSequences.getOrElse(Nil), reasoningEffort, provider, providerOptions,
        deferred
      )

      val textStream = events.collect:
        case e if e.eventType == StreamEventType.ContentDelta =>
          e.contentDelta
      .unNone

      StreamResult(
        events = events,
        response = deferred.get,
        textStream = textStream
      )

  // ---------------------------------------------------------------------------
  // Validation and message building (shared logic with Generate)
  // ---------------------------------------------------------------------------

  private def validateInputs(
      prompt: Option[String],
      messages: Option[List[Message]]
  ): IO[Unit] =
    (prompt, messages) match
      case (Some(_), Some(_)) =>
        IO.raiseError(
          new ConfigurationError("Provide either 'prompt' or 'messages', not both")
        )
      case (None, None) =>
        IO.raiseError(
          new ConfigurationError("Either 'prompt' or 'messages' must be provided")
        )
      case _ => IO.unit

  private def buildInitialMessages(
      system: Option[String],
      prompt: Option[String],
      messages: Option[List[Message]]
  ): List[Message] =
    val systemMsgs = system.map(Message.system).toList
    val conversationMsgs = prompt match
      case Some(p) => List(Message.user(p))
      case None    => messages.getOrElse(Nil)
    systemMsgs ++ conversationMsgs

  private def resolveClient(explicit: Option[Client]): IO[Client] =
    explicit match
      case Some(c) => IO.pure(c)
      case None    => DefaultClient.get

  // ---------------------------------------------------------------------------
  // Stream construction with tool-call loop
  // ---------------------------------------------------------------------------

  private def buildStream(
      client: Client,
      model: String,
      conversation: List[Message],
      toolDefs: List[ToolDefinition],
      toolChoice: Option[ToolChoice],
      toolIndex: Map[String, Json => IO[String]],
      remainingRounds: Int,
      responseFormat: Option[ResponseFormat],
      temperature: Option[Double],
      topP: Option[Double],
      maxTokens: Option[Int],
      stopSequences: List[String],
      reasoningEffort: Option[String],
      provider: Option[String],
      providerOptions: Option[Json],
      deferred: Deferred[IO, Response]
  ): Stream[IO, StreamEvent] =
    streamOneRound(
      client, model, conversation, toolDefs, toolChoice, toolIndex,
      remainingRounds, responseFormat, temperature, topP, maxTokens,
      stopSequences, reasoningEffort, provider, providerOptions, deferred
    )

  /** Stream a single model turn. After it completes, check if tool calls
    * need execution and, if so, recurse for the next round.
    */
  private def streamOneRound(
      client: Client,
      model: String,
      conversation: List[Message],
      toolDefs: List[ToolDefinition],
      toolChoice: Option[ToolChoice],
      toolIndex: Map[String, Json => IO[String]],
      remainingRounds: Int,
      responseFormat: Option[ResponseFormat],
      temperature: Option[Double],
      topP: Option[Double],
      maxTokens: Option[Int],
      stopSequences: List[String],
      reasoningEffort: Option[String],
      provider: Option[String],
      providerOptions: Option[Json],
      deferred: Deferred[IO, Response]
  ): Stream[IO, StreamEvent] =
    val request = Request(
      model = model,
      messages = conversation,
      provider = provider,
      tools = toolDefs,
      toolChoice = toolChoice,
      responseFormat = responseFormat,
      temperature = temperature,
      topP = topP,
      maxTokens = maxTokens,
      stopSequences = stopSequences,
      reasoningEffort = reasoningEffort,
      providerOptions = providerOptions
    )

    // Use an accumulator ref to collect the response from streaming events
    Stream.eval(Ref.of[IO, StreamAccumulator](new StreamAccumulator)).flatMap: accRef =>
      val rawStream = client.stream(request)

      // Tap each event through the accumulator, then emit it downstream
      val tapped = rawStream.evalTap: event =>
        accRef.update: acc =>
          acc.process(event)
          acc

      // After the raw stream ends, decide whether to continue with tool calls
      tapped ++ Stream.eval(accRef.get).flatMap: acc =>
        val calls = acc.toolCalls
        val executableCalls = calls.filter(c => toolIndex.contains(c.name))

        if executableCalls.isEmpty || remainingRounds <= 0 then
          // Terminal: complete the deferred with the accumulated response
          Stream.eval:
            acc.response match
              case Some(r) => deferred.complete(r).void
              case None    =>
                val empty = Response(
                  id = "", model = model, provider = "",
                  message = Message.assistant(""),
                  finishReason = FinishReason.stop, usage = Usage.empty
                )
                deferred.complete(empty).void
          .drain
        else
          // Execute tools and continue with next round
          Stream.eval:
            executableCalls.parTraverseN(8): call =>
              val handler = toolIndex(call.name)
              handler(call.arguments)
                .map(output => ToolResult(call.id, Json.fromString(output)))
                .handleError(err => ToolResult(call.id, Json.fromString(s"Error: ${err.getMessage}"), isError = true))
          .flatMap: results =>
            // Build updated conversation
            val assistantMsg = acc.response
              .map(_.message)
              .getOrElse(Message.assistant(acc.currentText))
            val toolResultMsgs = results.map: tr =>
              Message.toolResult(tr.toolCallId, tr.content, tr.isError)
            val updatedConversation = (conversation :+ assistantMsg) ::: toolResultMsgs

            streamOneRound(
              client, model, updatedConversation, toolDefs, toolChoice,
              toolIndex, remainingRounds - 1, responseFormat, temperature,
              topP, maxTokens, stopSequences, reasoningEffort, provider,
              providerOptions, deferred
            )
