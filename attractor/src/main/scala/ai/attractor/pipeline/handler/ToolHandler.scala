package ai.attractor.pipeline.handler

import cats.effect.IO

import java.nio.file.{Files, Paths}

import ai.attractor.pipeline.parser.{Graph, Node}
import ai.attractor.pipeline.state.{Context, Outcome, StageStatus}

// ---------------------------------------------------------------------------
// Tool handler -- executes shell commands
// ---------------------------------------------------------------------------

/** Executes a `tool_command` from node attributes via shell process.
  * Captures stdout/stderr and maps the exit code to Success/Fail.
  *
  * Node attributes:
  *   - `tool_command`: the shell command to execute (required)
  *   - `tool_timeout`: timeout in seconds (optional, default 60)
  *   - `tool_cwd`: working directory (optional)
  */
object ToolHandler extends Handler:

  def execute(node: Node, context: Context, graph: Graph, logsRoot: String): IO[Outcome] =
    val command = node.attributes.getOrElse("tool_command", "")
    if command.isEmpty then
      IO.pure(Outcome.fail(
        failureReason = s"Tool node '${node.id}' has no tool_command attribute"
      ))
    else
      val timeoutSec = node.attributes.get("tool_timeout").flatMap(_.toLongOption).getOrElse(60L)
      val cwd = node.attributes.get("tool_cwd").filter(_.nonEmpty)
      val stageDir = Paths.get(logsRoot, node.id)

      for
        _ <- IO.blocking(Files.createDirectories(stageDir))
        result <- runShellCommand(command, cwd, timeoutSec)
        (exitCode, stdout, stderr) = result
        _ <- IO.blocking(Files.writeString(stageDir.resolve("stdout.txt"), stdout))
        _ <- IO.blocking(Files.writeString(stageDir.resolve("stderr.txt"), stderr))
      yield
        if exitCode == 0 then
          val truncated = if stdout.length > 200 then stdout.take(200) + "..." else stdout
          Outcome.success(
            notes = s"Tool '${node.id}' completed (exit 0)",
            contextUpdates = Map(
              "last_stage" -> node.id,
              "tool.stdout" -> truncated,
              "tool.exit_code" -> "0"
            )
          )
        else
          Outcome.fail(
            failureReason = s"Tool '${node.id}' failed (exit $exitCode): ${stderr.take(200)}",
            contextUpdates = Map(
              "last_stage" -> node.id,
              "tool.stderr" -> stderr.take(200),
              "tool.exit_code" -> exitCode.toString
            )
          )

  private def runShellCommand(
      command: String,
      cwd: Option[String],
      timeoutSec: Long
  ): IO[(Int, String, String)] =
    IO.blocking:
      val pb = new ProcessBuilder("sh", "-c", command)
      cwd.foreach(d => pb.directory(new java.io.File(d)))
      pb.redirectErrorStream(false)
      val process = pb.start()

      // Read stdout and stderr concurrently to avoid deadlock
      val stdoutFuture = scala.concurrent.Future(
        new String(process.getInputStream.readAllBytes())
      )(scala.concurrent.ExecutionContext.global)
      val stderrFuture = scala.concurrent.Future(
        new String(process.getErrorStream.readAllBytes())
      )(scala.concurrent.ExecutionContext.global)

      val completed = process.waitFor(timeoutSec, java.util.concurrent.TimeUnit.SECONDS)
      if !completed then
        process.destroyForcibly()
        (137, "", s"Process timed out after ${timeoutSec}s")
      else
        import scala.concurrent.Await
        import scala.concurrent.duration.*
        val stdout = Await.result(stdoutFuture, 5.seconds)
        val stderr = Await.result(stderrFuture, 5.seconds)
        (process.exitValue(), stdout, stderr)
