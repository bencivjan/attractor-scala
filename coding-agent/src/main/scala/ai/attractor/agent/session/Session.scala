package ai.attractor.agent.session

import ai.attractor.llm.model.*
import ai.attractor.llm.provider.Client
import ai.attractor.agent.env.ExecutionEnvironment
import ai.attractor.agent.event.{EventEmitter, EventKind, SessionEvent}
import ai.attractor.agent.profile.{ProviderProfile, ProjectDocDiscovery}
import ai.attractor.agent.tool.Truncation
import cats.effect.{Async, Deferred, IO, Ref}
import cats.effect.std.Queue
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*

import java.time.Instant

class Session(
    val id: String,
    val providerProfile: ProviderProfile[IO],
    val executionEnv: ExecutionEnvironment[IO],
    val config: SessionConfig,
    val llmClient: Client,
    val eventEmitter: EventEmitter[IO],
    private val history: Ref[IO, List[Turn]],
    private val state: Ref[IO, SessionState],
    private val steeringQueue: Queue[IO, String],
    private val followupQueue: Queue[IO, String],
    private val abortSignal: Deferred[IO, Unit],
    private val totalUsage: Ref[IO, Usage]
):

  /** Main entry point: submit user input and run the agentic loop */
  def submit(input: String): IO[Unit] =
    for
      current <- state.get
      _ <- current match
        case SessionState.Closed =>
          IO.raiseError(new IllegalStateException("Session is closed"))
        case SessionState.Processing =>
          IO.raiseError(new IllegalStateException("Session is already processing"))
        case _ =>
          state.set(SessionState.Processing) *>
            eventEmitter.emit(SessionEvent(EventKind.UserInput, id, Map("content" -> input))) *>
            processInput(input).guarantee(
              state.set(SessionState.Idle)
            )
    yield ()

  /** Inject a steering message into the current processing loop */
  def steer(message: String): IO[Unit] =
    steeringQueue.offer(message)

  /** Queue a follow-up message for after current processing completes */
  def followUp(message: String): IO[Unit] =
    followupQueue.offer(message)

  /** Abort the current processing loop */
  def abort: IO[Unit] =
    // Deferred can only be completed once; if already completed this is a no-op
    abortSignal.complete(()).attempt.void

  /** Close the session and release resources */
  def close: IO[Unit] =
    for
      _ <- state.set(SessionState.Closed)
      _ <- eventEmitter.emit(SessionEvent(EventKind.SessionEnd, id))
    yield ()

  def currentState: IO[SessionState] = state.get

  def getHistory: IO[List[Turn]] = history.get

  // ---------------------------------------------------------------------------
  // Core agentic loop
  // ---------------------------------------------------------------------------

  private def processInput(input: String): IO[Unit] =
    for
      _ <- appendTurn(UserTurn(input))
      _ <- drainSteering
      _ <- agentLoop(toolRound = 0)
      // Check for follow-up messages
      followUp <- followupQueue.tryTake
      _ <- followUp match
        case Some(msg) => processInput(msg)
        case None      => IO.unit
    yield ()

  private def agentLoop(toolRound: Int): IO[Unit] =
    for
      // Check abort signal
      aborted <- abortSignal.tryGet
      _ <-
        if aborted.isDefined then
          appendTurn(SystemTurn("[Session aborted by user]"))
        else
          for
            // Check tool round limits
            _ <- checkToolRoundLimit(toolRound)
            // Check turn limits
            _ <- checkTurnLimit
            // Build and send LLM request
            response <- callLlm
            // Record assistant turn
            assistantTurn = AssistantTurn(
              content = response.text,
              toolCalls = response.toolCalls,
              reasoning = response.reasoning,
              usage = response.usage,
              responseId = Some(response.id)
            )
            _ <- appendTurn(assistantTurn)
            _ <- totalUsage.update(_ + response.usage)
            // Emit assistant text events
            _ <- emitAssistantEvents(assistantTurn)
            // Check context window usage
            _ <- checkContextWindow(response.usage)
            // If there are tool calls, execute them and continue the loop
            _ <-
              if response.toolCalls.isEmpty then IO.unit
              else
                for
                  results <- executeToolCalls(response.toolCalls)
                  _ <- appendTurn(ToolResultsTurn(results))
                  _ <- drainSteering
                  // Loop detection
                  loopDetected <- detectLoop
                  _ <-
                    if loopDetected then
                      eventEmitter.emit(SessionEvent(EventKind.LoopDetection, id)) *>
                        appendTurn(SystemTurn(
                          "[Loop detected: You appear to be repeating the same tool calls. " +
                          "Please try a different approach or ask the user for clarification.]"
                        ))
                    else
                      agentLoop(toolRound + 1)
                yield ()
          yield ()
    yield ()

  private def callLlm: IO[Response] =
    for
      messages <- convertHistoryToMessages
      projectDocs <- ProjectDocDiscovery.discover(executionEnv, providerProfile.projectDocFiles)
      systemPrompt <- providerProfile.buildSystemPrompt(executionEnv, projectDocs)
      request = Request(
        model = providerProfile.model,
        messages = Message.system(systemPrompt) :: messages,
        tools = providerProfile.tools,
        toolChoice = Some(ToolChoice.auto),
        reasoningEffort = config.reasoningEffort,
        providerOptions = providerProfile.providerOptions
      )
      response <- llmClient.complete(request)
    yield response

  // ---------------------------------------------------------------------------
  // Tool execution
  // ---------------------------------------------------------------------------

  private def executeToolCalls(toolCalls: List[ToolCall]): IO[List[ToolResult]] =
    if providerProfile.supportsParallelToolCalls then
      toolCalls.parTraverse(executeSingleTool)
    else
      toolCalls.traverse(executeSingleTool)

  private def executeSingleTool(toolCall: ToolCall): IO[ToolResult] =
    val startTime = Instant.now()
    for
      _ <- eventEmitter.emit(SessionEvent(
        EventKind.ToolCallStart, id,
        Map("tool_name" -> toolCall.name, "tool_call_id" -> toolCall.id)
      ))
      resultWithOutput <- providerProfile.toolRegistry.get(toolCall.name) match
        case Some(tool) =>
          tool.executor(toolCall.arguments, executionEnv)
            .map { output =>
              val truncated = Truncation.truncateToolOutput(output, toolCall.name, config)
              (ToolResult(toolCall.id, Json.fromString(truncated)), output)
            }
            .handleErrorWith { error =>
              val errMsg = s"Error: ${error.getMessage}"
              IO.pure((ToolResult(toolCall.id, Json.fromString(errMsg), isError = true), errMsg))
            }
        case None =>
          val errMsg = s"Error: Unknown tool '${toolCall.name}'"
          IO.pure((ToolResult(toolCall.id, Json.fromString(errMsg), isError = true), errMsg))
      (result, fullOutput) = resultWithOutput
      _ <- eventEmitter.emit(SessionEvent(
        EventKind.ToolCallEnd, id,
        Map(
          "tool_name" -> toolCall.name,
          "tool_call_id" -> toolCall.id,
          "is_error" -> result.isError,
          "duration_ms" -> java.time.Duration.between(startTime, Instant.now()).toMillis,
          "full_output" -> fullOutput
        )
      ))
    yield result

  // ---------------------------------------------------------------------------
  // History <-> Message conversion
  // ---------------------------------------------------------------------------

  private def convertHistoryToMessages: IO[List[Message]] =
    history.get.map { turns =>
      turns.flatMap {
        case UserTurn(content, _) =>
          List(Message.user(content))

        case AssistantTurn(content, toolCalls, reasoning, _, _, _) =>
          val parts = scala.collection.mutable.ListBuffer[ContentPart]()
          // Add thinking/reasoning parts first
          reasoning.foreach { r =>
            parts += ContentPart.thinking(ThinkingData(r))
          }
          // Add text content
          if content.nonEmpty then
            parts += ContentPart.text(content)
          // Add tool call parts
          toolCalls.foreach { tc =>
            parts += ContentPart.toolCall(ToolCallData(tc.id, tc.name, tc.arguments))
          }
          List(Message(Role.Assistant, parts.toList))

        case ToolResultsTurn(results, _) =>
          results.map { tr =>
            Message.toolResult(tr.toolCallId, tr.content, tr.isError)
          }

        case SystemTurn(content, _) =>
          List(Message.user(s"[System: $content]"))

        case SteeringTurn(content, _) =>
          List(Message.user(s"[Important guidance: $content]"))
      }
    }

  // ---------------------------------------------------------------------------
  // Steering
  // ---------------------------------------------------------------------------

  private def drainSteering: IO[Unit] =
    def drain: IO[Unit] =
      steeringQueue.tryTake.flatMap:
        case Some(msg) =>
          appendTurn(SteeringTurn(msg)) *>
            eventEmitter.emit(SessionEvent(EventKind.SteeringInjected, id, Map("content" -> msg))) *>
            drain
        case None => IO.unit
    drain

  // ---------------------------------------------------------------------------
  // Loop detection
  // ---------------------------------------------------------------------------

  private def detectLoop: IO[Boolean] =
    if !config.enableLoopDetection then IO.pure(false)
    else
      history.get.map { turns =>
        // Collect recent assistant turns (which carry tool call info)
        val recentAssistantTurns = turns
          .collect { case t: AssistantTurn if t.toolCalls.nonEmpty => t }
          .takeRight(config.loopDetectionWindow)

        if recentAssistantTurns.length < 4 then false
        else
          // Build a signature from tool names + argument keys (not IDs, which change every call)
          val toolSignatures = recentAssistantTurns.map { turn =>
            turn.toolCalls.map { tc =>
              val argKeys = tc.arguments.asObject.map(_.keys.toList.sorted.mkString(",")).getOrElse("")
              s"${tc.name}($argKeys)"
            }.sorted.mkString(";")
          }
          // Check for repeating pattern of length 2
          val half = toolSignatures.length / 2
          if half >= 2 then
            val first = toolSignatures.take(half)
            val second = toolSignatures.drop(half).take(half)
            first == second
          else false
      }

  // ---------------------------------------------------------------------------
  // Limit checks
  // ---------------------------------------------------------------------------

  private def checkTurnLimit: IO[Unit] =
    if config.maxTurns <= 0 then IO.unit
    else
      history.get.flatMap { turns =>
        val userTurns = turns.count(_.isInstanceOf[UserTurn])
        if userTurns > config.maxTurns then
          eventEmitter.emit(SessionEvent(EventKind.TurnLimit, id)) *>
            IO.raiseError(new RuntimeException(
              s"Maximum turn limit (${config.maxTurns}) reached"
            ))
        else IO.unit
      }

  private def checkToolRoundLimit(currentRound: Int): IO[Unit] =
    if config.maxToolRoundsPerInput <= 0 then IO.unit
    else if currentRound >= config.maxToolRoundsPerInput then
      eventEmitter.emit(SessionEvent(EventKind.TurnLimit, id,
        Map("reason" -> "tool_round_limit"))) *>
        appendTurn(SystemTurn(
          s"[Tool round limit (${config.maxToolRoundsPerInput}) reached. " +
          "Please provide your best response with the information gathered so far.]"
        ))
    else IO.unit

  private def checkContextWindow(lastUsage: Usage): IO[Unit] =
    val totalTokens = lastUsage.inputTokens
    val windowSize = providerProfile.contextWindowSize
    val threshold = (windowSize * 0.8).toInt
    if totalTokens >= threshold then
      eventEmitter.emit(SessionEvent(EventKind.Warning, id,
        Map("message" -> s"Context window ${(totalTokens.toDouble / windowSize * 100).toInt}% used ($totalTokens/$windowSize tokens)")))
    else IO.unit

  // ---------------------------------------------------------------------------
  // Event emission helpers
  // ---------------------------------------------------------------------------

  private def emitAssistantEvents(turn: AssistantTurn): IO[Unit] =
    for
      _ <- eventEmitter.emit(SessionEvent(EventKind.AssistantTextStart, id))
      _ <-
        if turn.content.nonEmpty then
          eventEmitter.emit(SessionEvent(EventKind.AssistantTextDelta, id,
            Map("text" -> turn.content)))
        else IO.unit
      _ <- eventEmitter.emit(SessionEvent(EventKind.AssistantTextEnd, id))
    yield ()

  // ---------------------------------------------------------------------------
  // Internal helpers
  // ---------------------------------------------------------------------------

  private def appendTurn(turn: Turn): IO[Unit] =
    history.update(_ :+ turn)

object Session:
  def create(
      id: String,
      providerProfile: ProviderProfile[IO],
      executionEnv: ExecutionEnvironment[IO],
      config: SessionConfig,
      llmClient: Client,
      eventEmitter: EventEmitter[IO]
  ): IO[Session] =
    for
      historyRef <- Ref.of[IO, List[Turn]](Nil)
      stateRef <- Ref.of[IO, SessionState](SessionState.Idle)
      steeringQ <- Queue.unbounded[IO, String]
      followupQ <- Queue.unbounded[IO, String]
      abortDef <- Deferred[IO, Unit]
      usageRef <- Ref.of[IO, Usage](Usage.empty)
      _ <- eventEmitter.emit(SessionEvent(EventKind.SessionStart, id))
    yield new Session(
      id = id,
      providerProfile = providerProfile,
      executionEnv = executionEnv,
      config = config,
      llmClient = llmClient,
      eventEmitter = eventEmitter,
      history = historyRef,
      state = stateRef,
      steeringQueue = steeringQ,
      followupQueue = followupQ,
      abortSignal = abortDef,
      totalUsage = usageRef
    )
