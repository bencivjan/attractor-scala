package ai.attractor.pipeline.handler

import cats.effect.IO

import java.nio.file.{Files, Path, Paths}

import io.circe.syntax.*

import ai.attractor.pipeline.parser.{Graph, Node}
import ai.attractor.pipeline.state.{Context, Outcome, StageStatus}

// ---------------------------------------------------------------------------
// LLM code-generation backend trait
// ---------------------------------------------------------------------------

/** Pluggable backend for LLM-powered code generation stages.
  * Returns either a terminal Outcome (Left) on unrecoverable failure,
  * or the raw response text (Right) on success.
  */
trait CodergenBackend:
  def run(node: Node, prompt: String, context: Context): IO[Either[Outcome, String]]

// ---------------------------------------------------------------------------
// Codergen handler -- LLM task execution
// ---------------------------------------------------------------------------

/** Executes an LLM code-generation stage:
  *   1. Builds a prompt from the node's `prompt` attribute (falls back to `label`)
  *   2. Expands the `$goal` variable from context
  *   3. Creates a stage directory and writes `prompt.md`
  *   4. Invokes the backend (or simulates if absent)
  *   5. Writes `response.md` and `status.json`
  *   6. Returns an Outcome with `last_stage` and `last_response` context updates
  */
class CodergenHandler(backend: Option[CodergenBackend] = None) extends Handler:

  def execute(node: Node, context: Context, graph: Graph, logsRoot: String): IO[Outcome] =
    val stageDir = Paths.get(logsRoot, node.id)
    for
      prompt <- buildPrompt(node, context, graph)
      _ <- IO.blocking(Files.createDirectories(stageDir))
      _ <- writeFile(stageDir.resolve("prompt.md"), prompt)
      result <- callBackend(node, prompt, context)
      outcome = result match
        case Left(o)         => o
        case Right(response) => buildSuccessOutcome(node, response)
      responseText = result.fold(_.notes, identity)
      _ <- writeFile(stageDir.resolve("response.md"), responseText)
      _ <- writeFile(stageDir.resolve("status.json"), outcome.asJson.noSpaces)
    yield outcome

  private def buildPrompt(node: Node, context: Context, graph: Graph): IO[String] =
    val raw = if node.prompt.nonEmpty then node.prompt else node.label
    context.getString("goal").map: goal =>
      val effectiveGoal = if goal.nonEmpty then goal else graph.goal
      raw.replace("$goal", effectiveGoal)

  private def callBackend(
      node: Node,
      prompt: String,
      context: Context
  ): IO[Either[Outcome, String]] =
    backend match
      case Some(b) => b.run(node, prompt, context)
      case None    => IO.pure(Right(s"[simulated] Response for: ${prompt.take(100)}"))

  private def buildSuccessOutcome(node: Node, response: String): Outcome =
    val truncated =
      if response.length > 200 then response.take(200) + "..."
      else response
    Outcome.success(
      notes = s"Codergen stage '${node.id}' completed",
      contextUpdates = Map(
        "last_stage" -> node.id,
        "last_response" -> truncated
      )
    )

  private def writeFile(path: Path, content: String): IO[Unit] =
    IO.blocking(Files.writeString(path, content)).void
