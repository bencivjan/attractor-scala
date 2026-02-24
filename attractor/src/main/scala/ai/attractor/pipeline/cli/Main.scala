package ai.attractor.pipeline.cli

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all.*
import com.monovore.decline.*
import com.monovore.decline.effect.CommandIOApp

import java.nio.file.{Files, Paths}

import ai.attractor.llm.provider.Client
import ai.attractor.pipeline.engine.{PipelineConfig, PipelineRunner}
import ai.attractor.pipeline.handler.UnifiedLlmBackend
import ai.attractor.pipeline.interviewer.{AutoApproveInterviewer, ConsoleInterviewer, Interviewer}
import ai.attractor.pipeline.pipelines.Pipelines
import ai.attractor.pipeline.state.StageStatus

// ---------------------------------------------------------------------------
// CLI entry point for the Attractor pipeline engine
// ---------------------------------------------------------------------------

object Main extends CommandIOApp(
  name = "attractor",
  header = "Attractor pipeline engine CLI"
):

  // -------------------------------------------------------------------------
  // list subcommand
  // -------------------------------------------------------------------------

  private val listCmd: Opts[IO[ExitCode]] =
    Opts.subcommand("list", "List available pipelines"):
      Opts(listPipelines)

  private def listPipelines: IO[ExitCode] =
    IO.blocking:
      val names = Pipelines.names
      println("Available pipelines:")
      names.foreach(n => println(s"  $n"))
    .as(ExitCode.Success)

  // -------------------------------------------------------------------------
  // run subcommand
  // -------------------------------------------------------------------------

  private val pipelineArg: Opts[String] =
    Opts.argument[String](metavar = "pipeline")

  private val goalOpt: Opts[String] =
    Opts.option[String]("goal", help = "Goal/task description for the pipeline")

  private val logsDirOpt: Opts[String] =
    Opts.option[String]("logs", help = "Logs directory")
      .withDefault(".attractor/logs")

  private val autoApproveFlag: Opts[Boolean] =
    Opts.flag("auto-approve", help = "Auto-approve all human-in-the-loop prompts")
      .orFalse

  private val interactiveFlag: Opts[Boolean] =
    Opts.flag("interactive", help = "Use interactive console interviewer (default)")
      .orFalse

  private val checkpointOpt: Opts[Option[String]] =
    Opts.option[String]("checkpoint", help = "Resume from a checkpoint file")
      .orNone

  private val maxStepsOpt: Opts[Option[Int]] =
    Opts.option[Int]("max-steps", help = "Maximum execution steps")
      .orNone

  private val runCmd: Opts[IO[ExitCode]] =
    Opts.subcommand("run", "Run a pipeline"):
      (pipelineArg, goalOpt, logsDirOpt, autoApproveFlag, interactiveFlag, checkpointOpt, maxStepsOpt)
        .mapN(runPipeline)

  private def runPipeline(
      pipeline: String,
      goal: String,
      logsDir: String,
      autoApprove: Boolean,
      interactive: Boolean,
      checkpoint: Option[String],
      maxSteps: Option[Int]
  ): IO[ExitCode] =
    val interviewer: Interviewer =
      if autoApprove then AutoApproveInterviewer
      else ConsoleInterviewer

    val config = PipelineConfig(
      checkpointPath = checkpoint,
      maxSteps = maxSteps.getOrElse(1000)
    )

    val dotSourceIO: IO[String] = loadDotSource(pipeline)

    val hasApiKeys =
      Option(System.getenv("ANTHROPIC_API_KEY")).exists(_.nonEmpty) ||
      Option(System.getenv("OPENAI_API_KEY")).exists(_.nonEmpty) ||
      Option(System.getenv("GEMINI_API_KEY")).exists(_.nonEmpty) ||
      Option(System.getenv("GOOGLE_API_KEY")).exists(_.nonEmpty)

    for
      dotSource <- dotSourceIO
      // Inject goal into the DOT source via context
      dotWithGoal = dotSource
      _ <- IO.blocking(Files.createDirectories(Paths.get(logsDir)))
      outcome <-
        if hasApiKeys then
          Client.fromEnv().use: client =>
            val backend = UnifiedLlmBackend(client)
            PipelineRunner.fromDotSource(
              dotSource = dotWithGoal,
              logsRoot = logsDir,
              backend = Some(backend),
              interviewer = interviewer,
              config = config
            )
        else
          IO.blocking(System.err.println(
            "Warning: No API keys found, running in simulated mode"
          )) *>
          PipelineRunner.fromDotSource(
            dotSource = dotWithGoal,
            logsRoot = logsDir,
            backend = None,
            interviewer = interviewer,
            config = config
          )
      _ <- IO.blocking:
        outcome.status match
          case StageStatus.Success =>
            println(s"Pipeline completed successfully: ${outcome.notes}")
          case StageStatus.Fail =>
            System.err.println(s"Pipeline failed: ${outcome.failureReason}")
          case other =>
            println(s"Pipeline finished with status: $other — ${outcome.notes}")
    yield
      if outcome.status == StageStatus.Success then ExitCode.Success
      else ExitCode(1)

  private def loadDotSource(pipeline: String): IO[String] =
    if pipeline.endsWith(".dot") then
      val path = Paths.get(pipeline)
      IO.blocking:
        if !Files.exists(path) then
          throw new IllegalArgumentException(s"DOT file not found: $pipeline")
        new String(Files.readAllBytes(path), "UTF-8")
    else
      Pipelines.get(pipeline) match
        case Some(dot) => IO.pure(dot)
        case None =>
          IO.raiseError(new IllegalArgumentException(
            s"Unknown pipeline '$pipeline'. Available: ${Pipelines.names.mkString(", ")}"
          ))

  // -------------------------------------------------------------------------
  // Wire up
  // -------------------------------------------------------------------------

  override def main: Opts[IO[ExitCode]] =
    listCmd orElse runCmd
