package ai.attractor.agent.session

import ai.attractor.llm.model.ToolDefinition
import ai.attractor.llm.provider.Client
import ai.attractor.agent.env.ExecutionEnvironment
import ai.attractor.agent.event.{EventEmitter, QueueEventEmitter}
import ai.attractor.agent.profile.ProviderProfile
import ai.attractor.agent.tool.RegisteredTool
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*

import java.util.UUID

// ---------------------------------------------------------------------------
// SubAgent types
// ---------------------------------------------------------------------------

enum SubAgentStatus:
  case Running, Completed, Failed, Closed

case class SubAgentHandle(
    id: String,
    session: Session,
    status: Ref[IO, SubAgentStatus]
)

case class SubAgentResult(
    output: String,
    success: Boolean,
    turnsUsed: Int
)

// ---------------------------------------------------------------------------
// SubAgent manager
// ---------------------------------------------------------------------------

class SubAgentManager(
    private[session] val parentSession: Session,
    private val maxDepth: Int,
    private val currentDepth: Int,
    private val agents: Ref[IO, Map[String, SubAgentHandle]]
):

  def spawn(
      task: String,
      providerProfile: ProviderProfile[IO],
      executionEnv: ExecutionEnvironment[IO],
      llmClient: Client,
      config: SessionConfig
  ): IO[SubAgentHandle] =
    if currentDepth >= maxDepth then
      IO.raiseError(new RuntimeException(
        s"Maximum subagent depth ($maxDepth) reached. Cannot spawn further subagents."
      ))
    else
      for
        emitter <- QueueEventEmitter.create
        agentId = UUID.randomUUID().toString.take(8)
        session <- Session.create(
          id = s"sub-$agentId",
          providerProfile = providerProfile,
          executionEnv = executionEnv,
          config = config.copy(
            maxSubagentDepth = maxDepth - currentDepth - 1
          ),
          llmClient = llmClient,
          eventEmitter = emitter
        )
        statusRef <- Ref.of[IO, SubAgentStatus](SubAgentStatus.Running)
        handle = SubAgentHandle(agentId, session, statusRef)
        _ <- agents.update(_ + (agentId -> handle))
        // Start the subagent processing in the background
        _ <- (session.submit(task) *>
          statusRef.set(SubAgentStatus.Completed))
          .handleErrorWith(_ => statusRef.set(SubAgentStatus.Failed))
          .start
      yield handle

  def sendInput(agentId: String, input: String): IO[Unit] =
    for
      agentMap <- agents.get
      handle <- agentMap.get(agentId) match
        case Some(h) => IO.pure(h)
        case None => IO.raiseError(new RuntimeException(s"No subagent with id '$agentId'"))
      _ <- handle.session.followUp(input)
    yield ()

  def waitForAgent(agentId: String): IO[SubAgentResult] =
    for
      agentMap <- agents.get
      handle <- agentMap.get(agentId) match
        case Some(h) => IO.pure(h)
        case None => IO.raiseError(new RuntimeException(s"No subagent with id '$agentId'"))
      // Poll until the agent is no longer running
      result <- pollUntilDone(handle)
    yield result

  private def pollUntilDone(handle: SubAgentHandle): IO[SubAgentResult] =
    handle.status.get.flatMap:
      case SubAgentStatus.Running =>
        IO.sleep(scala.concurrent.duration.FiniteDuration(100, "ms")) *> pollUntilDone(handle)
      case status =>
        for
          history <- handle.session.getHistory
          lastAssistant = history.collect { case a: AssistantTurn => a }.lastOption
          output = lastAssistant.map(_.content).getOrElse("")
          turnsUsed = history.count(_.isInstanceOf[UserTurn])
          success = status == SubAgentStatus.Completed
        yield SubAgentResult(output, success, turnsUsed)

  def closeAgent(agentId: String): IO[Unit] =
    for
      agentMap <- agents.get
      handle <- agentMap.get(agentId) match
        case Some(h) => IO.pure(h)
        case None => IO.raiseError(new RuntimeException(s"No subagent with id '$agentId'"))
      _ <- handle.session.close
      _ <- handle.status.set(SubAgentStatus.Closed)
      _ <- agents.update(_ - agentId)
    yield ()

  def closeAll: IO[Unit] =
    for
      agentMap <- agents.get
      _ <- agentMap.keys.toList.traverse_(closeAgent)
    yield ()

object SubAgentManager:
  def create(parentSession: Session, maxDepth: Int, currentDepth: Int = 0): IO[SubAgentManager] =
    Ref.of[IO, Map[String, SubAgentHandle]](Map.empty).map { agentsRef =>
      new SubAgentManager(parentSession, maxDepth, currentDepth, agentsRef)
    }

// ---------------------------------------------------------------------------
// SubAgent tool factories
// ---------------------------------------------------------------------------

object SubAgentTools:
  import cats.syntax.all.*

  def spawnAgentTool(manager: SubAgentManager): RegisteredTool[IO] = RegisteredTool(
    definition = ToolDefinition(
      name = "spawn_agent",
      description = "Spawn a subagent to work on a delegated task. The subagent runs independently with its own tool access and returns a handle ID for monitoring.",
      parameters = Json.obj(
        "type" -> "object".asJson,
        "properties" -> Json.obj(
          "task" -> Json.obj(
            "type" -> "string".asJson,
            "description" -> "The task description for the subagent to work on".asJson
          )
        ),
        "required" -> List("task").asJson
      )
    ),
    executor = (args, env) =>
      val task = args.hcursor.get[String]("task").getOrElse("")
      manager.spawn(
        task = task,
        providerProfile = manager.parentSession.providerProfile,
        executionEnv = manager.parentSession.executionEnv,
        llmClient = manager.parentSession.llmClient,
        config = manager.parentSession.config
      ).map(handle => s"Subagent spawned with id: ${handle.id}")
  )

  def sendInputTool(manager: SubAgentManager): RegisteredTool[IO] = RegisteredTool(
    definition = ToolDefinition(
      name = "send_input",
      description = "Send additional input to a running subagent.",
      parameters = Json.obj(
        "type" -> "object".asJson,
        "properties" -> Json.obj(
          "agent_id" -> Json.obj(
            "type" -> "string".asJson,
            "description" -> "The subagent handle ID".asJson
          ),
          "input" -> Json.obj(
            "type" -> "string".asJson,
            "description" -> "The input to send to the subagent".asJson
          )
        ),
        "required" -> List("agent_id", "input").asJson
      )
    ),
    executor = (args, _) =>
      val agentId = args.hcursor.get[String]("agent_id").getOrElse("")
      val input = args.hcursor.get[String]("input").getOrElse("")
      manager.sendInput(agentId, input).map(_ => "Input sent successfully")
  )

  def waitTool(manager: SubAgentManager): RegisteredTool[IO] = RegisteredTool(
    definition = ToolDefinition(
      name = "wait_agent",
      description = "Wait for a subagent to complete and return its result.",
      parameters = Json.obj(
        "type" -> "object".asJson,
        "properties" -> Json.obj(
          "agent_id" -> Json.obj(
            "type" -> "string".asJson,
            "description" -> "The subagent handle ID to wait for".asJson
          )
        ),
        "required" -> List("agent_id").asJson
      )
    ),
    executor = (args, _) =>
      val agentId = args.hcursor.get[String]("agent_id").getOrElse("")
      manager.waitForAgent(agentId).map { result =>
        val status = if result.success then "completed" else "failed"
        s"Subagent $status after ${result.turnsUsed} turns.\n\nOutput:\n${result.output}"
      }
  )

  def closeAgentTool(manager: SubAgentManager): RegisteredTool[IO] = RegisteredTool(
    definition = ToolDefinition(
      name = "close_agent",
      description = "Close and clean up a subagent.",
      parameters = Json.obj(
        "type" -> "object".asJson,
        "properties" -> Json.obj(
          "agent_id" -> Json.obj(
            "type" -> "string".asJson,
            "description" -> "The subagent handle ID to close".asJson
          )
        ),
        "required" -> List("agent_id").asJson
      )
    ),
    executor = (args, _) =>
      val agentId = args.hcursor.get[String]("agent_id").getOrElse("")
      manager.closeAgent(agentId).map(_ => s"Subagent $agentId closed")
  )

  def allTools(manager: SubAgentManager): List[RegisteredTool[IO]] =
    List(
      spawnAgentTool(manager),
      sendInputTool(manager),
      waitTool(manager),
      closeAgentTool(manager)
    )
