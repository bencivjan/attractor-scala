package ai.attractor.llm.api

import ai.attractor.llm.model.*
import ai.attractor.llm.provider.Client
import ai.attractor.llm.util.RetryPolicy
import cats.effect.IO
import cats.syntax.all.*
import cats.effect.syntax.all.*
import io.circe.Json
import io.circe.syntax.*

/** A tool definition enriched with an optional server-side execute handler.
  *
  * When `execute` is provided, the generate loop will automatically invoke it
  * for matching tool calls and feed results back to the model.
  */
final case class Tool(
    name: String,
    description: String,
    parameters: Json,
    execute: Option[Json => IO[String]] = None
):
  /** Convert to the wire-level ToolDefinition for provider requests. */
  def toDefinition: ToolDefinition =
    ToolDefinition(name, description, parameters)

/** High-level generate function that wraps the provider Client with an
  * automatic tool-execution loop, retry logic, and usage aggregation.
  */
object Generate:

  def apply(
      model: String,
      prompt: Option[String] = None,
      messages: Option[List[Message]] = None,
      system: Option[String] = None,
      tools: Option[List[Tool]] = None,
      toolChoice: Option[ToolChoice] = None,
      maxToolRounds: Int = 1,
      stopWhen: Option[List[StepResult] => Boolean] = None,
      responseFormat: Option[ResponseFormat] = None,
      temperature: Option[Double] = None,
      topP: Option[Double] = None,
      maxTokens: Option[Int] = None,
      stopSequences: Option[List[String]] = None,
      reasoningEffort: Option[String] = None,
      provider: Option[String] = None,
      providerOptions: Option[Json] = None,
      maxRetries: Int = 2,
      client: Option[Client] = None
  ): IO[GenerateResult] =
    for
      _         <- validateInputs(prompt, messages)
      resolved  <- resolveClient(client)
      initial   <- IO.pure(buildInitialMessages(system, prompt, messages))
      toolDefs   = tools.map(_.map(_.toDefinition)).getOrElse(Nil)
      toolIndex  = tools.map(_.flatMap(t => t.execute.map(t.name -> _)).toMap).getOrElse(Map.empty)
      policy     = RetryPolicy(maxRetries = maxRetries)
      result    <- RetryPolicy.retry(policy):
                     loop(
                       resolved, model, initial, toolDefs, toolChoice, toolIndex,
                       maxToolRounds, stopWhen.getOrElse(_ => false),
                       responseFormat, temperature, topP, maxTokens,
                       stopSequences.getOrElse(Nil), reasoningEffort,
                       provider, providerOptions,
                       steps = Nil, totalUsage = Usage.empty
                     )
    yield result

  // ---------------------------------------------------------------------------
  // Input validation
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

  // ---------------------------------------------------------------------------
  // Message construction
  // ---------------------------------------------------------------------------

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

  // ---------------------------------------------------------------------------
  // Client resolution
  // ---------------------------------------------------------------------------

  private def resolveClient(explicit: Option[Client]): IO[Client] =
    explicit match
      case Some(c) => IO.pure(c)
      case None    => DefaultClient.get

  // ---------------------------------------------------------------------------
  // Core tool-execution loop
  // ---------------------------------------------------------------------------

  private def loop(
      client: Client,
      model: String,
      conversation: List[Message],
      toolDefs: List[ToolDefinition],
      toolChoice: Option[ToolChoice],
      toolIndex: Map[String, Json => IO[String]],
      remainingRounds: Int,
      stopWhen: List[StepResult] => Boolean,
      responseFormat: Option[ResponseFormat],
      temperature: Option[Double],
      topP: Option[Double],
      maxTokens: Option[Int],
      stopSequences: List[String],
      reasoningEffort: Option[String],
      provider: Option[String],
      providerOptions: Option[Json],
      steps: List[StepResult],
      totalUsage: Usage
  ): IO[GenerateResult] =
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

    for
      response   <- client.complete(request)
      calls       = response.toolCalls
      reasoning   = response.reasoning
      newUsage    = totalUsage + response.usage

      // Execute tool calls if handlers are available
      results    <- executeToolCalls(calls, toolIndex)

      step = StepResult(
        text = response.text,
        reasoning = reasoning,
        toolCalls = calls,
        toolResults = results,
        finishReason = response.finishReason,
        usage = response.usage,
        response = response,
        warnings = response.warnings
      )

      allSteps = steps :+ step

      result <- shouldContinue(calls, results, toolIndex, remainingRounds, stopWhen, allSteps) match
        case false =>
          IO.pure(buildFinalResult(allSteps, newUsage, response))
        case true =>
          // Build the assistant message preserving all content parts from the response
          val assistantMsg = response.message
          val toolResultMsgs = results.map: tr =>
            Message.toolResult(tr.toolCallId, tr.content, tr.isError)
          val updatedConversation = (conversation :+ assistantMsg) ::: toolResultMsgs

          loop(
            client, model, updatedConversation, toolDefs, toolChoice,
            toolIndex, remainingRounds - 1, stopWhen,
            responseFormat, temperature, topP, maxTokens,
            stopSequences, reasoningEffort, provider, providerOptions,
            allSteps, newUsage
          )
    yield result

  // ---------------------------------------------------------------------------
  // Tool execution
  // ---------------------------------------------------------------------------

  private def executeToolCalls(
      calls: List[ToolCall],
      toolIndex: Map[String, Json => IO[String]]
  ): IO[List[ToolResult]] =
    val executableCalls = calls.filter(c => toolIndex.contains(c.name))
    executableCalls.parTraverseN(8): call =>
      val handler = toolIndex(call.name)
      handler(call.arguments)
        .map: output =>
          ToolResult(call.id, Json.fromString(output))
        .handleError: err =>
          ToolResult(call.id, Json.fromString(s"Error: ${err.getMessage}"), isError = true)

  // ---------------------------------------------------------------------------
  // Loop termination logic
  // ---------------------------------------------------------------------------

  private def shouldContinue(
      calls: List[ToolCall],
      results: List[ToolResult],
      toolIndex: Map[String, Json => IO[String]],
      remainingRounds: Int,
      stopWhen: List[StepResult] => Boolean,
      steps: List[StepResult]
  ): Boolean =
    if calls.isEmpty then false
    else if results.isEmpty then false
    else if remainingRounds <= 0 then false
    else if stopWhen(steps) then false
    else true

  // ---------------------------------------------------------------------------
  // Final result construction
  // ---------------------------------------------------------------------------

  private def buildFinalResult(
      steps: List[StepResult],
      totalUsage: Usage,
      lastResponse: Response
  ): GenerateResult =
    val lastStep = steps.last
    val allToolResults = steps.flatMap(_.toolResults)
    val allText = steps.map(_.text).mkString
    val allReasoning =
      val parts = steps.flatMap(_.reasoning)
      Option.when(parts.nonEmpty)(parts.mkString)

    GenerateResult(
      text = allText,
      reasoning = allReasoning,
      toolCalls = lastStep.toolCalls,
      toolResults = allToolResults,
      finishReason = lastStep.finishReason,
      usage = lastStep.usage,
      totalUsage = totalUsage,
      steps = steps,
      response = lastResponse
    )
