package ai.attractor.agent.profile

import ai.attractor.llm.model.ToolDefinition
import ai.attractor.agent.env.ExecutionEnvironment
import ai.attractor.agent.tool.{ToolRegistry, CoreTools, RegisteredTool}
import cats.effect.IO
import io.circe.Json
import io.circe.syntax.*

trait ProviderProfile[F[_]]:
  def id: String
  def model: String
  def toolRegistry: ToolRegistry[F]
  def buildSystemPrompt(env: ExecutionEnvironment[F], projectDocs: List[String]): F[String]
  def tools: List[ToolDefinition]
  def providerOptions: Option[Json]
  def supportsReasoning: Boolean
  def supportsStreaming: Boolean
  def supportsParallelToolCalls: Boolean
  def contextWindowSize: Int
  /** File names this profile loads for project documentation.
    * AGENTS.md is always loaded regardless of profile.
    */
  def projectDocFiles: List[String]

// ---------------------------------------------------------------------------
// Shared environment context block builder
// ---------------------------------------------------------------------------

private[profile] object EnvironmentContext:
  def buildBlock(env: ExecutionEnvironment[IO], modelName: String): IO[String] =
    import java.time.LocalDate
    for
      isGit <- IO.blocking {
        java.nio.file.Files.isDirectory(
          java.nio.file.Paths.get(env.workingDirectory, ".git")
        )
      }
      gitBranch <- if isGit then
        IO.blocking {
          val pb = new ProcessBuilder("git", "rev-parse", "--abbrev-ref", "HEAD")
          pb.directory(java.io.File(env.workingDirectory))
          val p = pb.start()
          val branch = new String(p.getInputStream.readAllBytes()).trim
          p.waitFor(3000, java.util.concurrent.TimeUnit.MILLISECONDS)
          branch
        }.handleError(_ => "unknown")
      else IO.pure("n/a")
    yield
      s"""<environment>
         |Working directory: ${env.workingDirectory}
         |Is git repository: $isGit
         |Git branch: $gitBranch
         |Platform: ${env.platform}
         |OS version: ${env.osVersion}
         |Today's date: ${LocalDate.now()}
         |Model: $modelName
         |</environment>""".stripMargin

// ---------------------------------------------------------------------------
// Project document discovery
// ---------------------------------------------------------------------------

object ProjectDocDiscovery:
  private val MaxBytes = 32 * 1024

  /** Discover and load project documentation files from the working directory
    * tree, respecting the profile's accepted file list.
    */
  def discover(env: ExecutionEnvironment[IO], acceptedFiles: List[String]): IO[List[String]] =
    import java.nio.file.{Files, Path, Paths}
    IO.blocking {
      val cwd = Paths.get(env.workingDirectory)
      val docs = scala.collection.mutable.ListBuffer[String]()
      var totalBytes = 0

      // Walk from cwd upward to find git root, collecting doc files at each level
      def findGitRoot(p: Path): Path =
        if Files.isDirectory(p.resolve(".git")) then p
        else
          val parent = p.getParent
          if parent == null then cwd else findGitRoot(parent)

      val root = findGitRoot(cwd)

      // Collect paths from root to cwd
      val pathSegments = scala.collection.mutable.ListBuffer[Path](root)
      var current = root
      val cwdNormalized = cwd.normalize()
      while current.normalize() != cwdNormalized do
        val relative = current.relativize(cwdNormalized)
        val next = current.resolve(relative.getName(0))
        pathSegments += next
        current = next

      for
        dir <- pathSegments.toList
        fileName <- acceptedFiles
      do
        val filePath = dir.resolve(fileName)
        if Files.isRegularFile(filePath) && totalBytes < MaxBytes then
          val content = Files.readString(filePath)
          val available = MaxBytes - totalBytes
          if content.length <= available then
            docs += s"# $fileName (from ${dir.relativize(filePath)})\n$content"
            totalBytes += content.length
          else
            docs += s"# $fileName (from ${dir.relativize(filePath)})\n${content.take(available)}\n[Project instructions truncated at 32KB]"
            totalBytes = MaxBytes

      docs.toList
    }.handleError(_ => Nil)

// ---------------------------------------------------------------------------
// Anthropic Profile
// ---------------------------------------------------------------------------

class AnthropicProfile(
    val model: String = "claude-sonnet-4-20250514",
    val contextWindowSize: Int = 200000
) extends ProviderProfile[IO]:

  val id: String = "anthropic"

  val supportsReasoning: Boolean = true
  val supportsStreaming: Boolean = true
  val supportsParallelToolCalls: Boolean = true

  val projectDocFiles: List[String] = List("AGENTS.md", "CLAUDE.md")

  val toolRegistry: ToolRegistry[IO] =
    val registry = ToolRegistry.empty[IO]
    registry.registerAll(CoreTools.all)
    registry

  def tools: List[ToolDefinition] = toolRegistry.definitions

  def providerOptions: Option[Json] = None

  def buildSystemPrompt(env: ExecutionEnvironment[IO], projectDocs: List[String]): IO[String] =
    EnvironmentContext.buildBlock(env, model).map: envBlock =>
      val docsSection =
        if projectDocs.isEmpty then ""
        else "\n\nProject documentation:\n" + projectDocs.mkString("\n---\n")

      s"""You are an expert software engineer. You have access to tools for reading, writing, and editing files, executing shell commands, and searching codebases.
         |
         |$envBlock
         |
         |Guidelines:
         |- Read files before editing to understand existing code and conventions
         |- Use exact string matching for edits (old_string must be unique in the file); include enough context for unique matches
         |- Prefer editing existing files over creating new ones unless explicitly needed
         |- Write clean, idiomatic code following existing project patterns
         |- Handle errors gracefully and validate inputs
         |- Run tests after making changes when a test suite exists
         |- Use absolute file paths$docsSection""".stripMargin

// ---------------------------------------------------------------------------
// OpenAI Profile
// ---------------------------------------------------------------------------

class OpenAIProfile(
    val model: String = "gpt-4o",
    val contextWindowSize: Int = 128000
) extends ProviderProfile[IO]:

  val id: String = "openai"

  val supportsReasoning: Boolean = false
  val supportsStreaming: Boolean = true
  val supportsParallelToolCalls: Boolean = true

  val projectDocFiles: List[String] = List("AGENTS.md", ".codex/instructions.md")

  val toolRegistry: ToolRegistry[IO] =
    val registry = ToolRegistry.empty[IO]
    // OpenAI uses write_file for edits instead of edit_file
    registry.registerAll(CoreTools.withoutEdit :+ applyPatchTool)
    registry

  def tools: List[ToolDefinition] = toolRegistry.definitions

  def providerOptions: Option[Json] = None

  def buildSystemPrompt(env: ExecutionEnvironment[IO], projectDocs: List[String]): IO[String] =
    EnvironmentContext.buildBlock(env, model).map: envBlock =>
      val docsSection =
        if projectDocs.isEmpty then ""
        else "\n\nProject documentation:\n" + projectDocs.mkString("\n---\n")

      s"""You are an expert software engineer. You have access to tools for reading and writing files, executing shell commands, and searching codebases.
         |
         |$envBlock
         |
         |Guidelines:
         |- Read files before modifying to understand existing patterns
         |- Use apply_patch to modify files by providing the complete new content
         |- Write clean, idiomatic code following project conventions
         |- Handle errors gracefully and validate inputs
         |- Run tests after changes when possible
         |- Use absolute file paths$docsSection""".stripMargin

  /** OpenAI-style patch tool that applies unified-diff-like patches via write_file */
  private val applyPatchTool: RegisteredTool[IO] = RegisteredTool(
    definition = ToolDefinition(
      name = "apply_patch",
      description = "Apply a patch to a file. Read the file first, then provide the complete new content.",
      parameters = Json.obj(
        "type" -> "object".asJson,
        "properties" -> Json.obj(
          "file_path" -> Json.obj(
            "type" -> "string".asJson,
            "description" -> "The absolute path to the file to patch".asJson
          ),
          "content" -> Json.obj(
            "type" -> "string".asJson,
            "description" -> "The complete new file content".asJson
          )
        ),
        "required" -> List("file_path", "content").asJson
      )
    ),
    executor = (args, env) =>
      val filePath = args.hcursor.get[String]("file_path").getOrElse("")
      val content = args.hcursor.get[String]("content").getOrElse("")
      env.writeFile(filePath, content).map(_ => s"Successfully patched $filePath")
  )

// ---------------------------------------------------------------------------
// Gemini Profile
// ---------------------------------------------------------------------------

class GeminiProfile(
    val model: String = "gemini-2.0-flash",
    val contextWindowSize: Int = 1000000
) extends ProviderProfile[IO]:

  val id: String = "gemini"

  val supportsReasoning: Boolean = false
  val supportsStreaming: Boolean = true
  val supportsParallelToolCalls: Boolean = false

  val projectDocFiles: List[String] = List("AGENTS.md", "GEMINI.md")

  val toolRegistry: ToolRegistry[IO] =
    val registry = ToolRegistry.empty[IO]
    registry.registerAll(CoreTools.all)
    registry

  def tools: List[ToolDefinition] = toolRegistry.definitions

  def providerOptions: Option[Json] = None

  def buildSystemPrompt(env: ExecutionEnvironment[IO], projectDocs: List[String]): IO[String] =
    EnvironmentContext.buildBlock(env, model).map: envBlock =>
      val docsSection =
        if projectDocs.isEmpty then ""
        else "\n\nProject documentation:\n" + projectDocs.mkString("\n---\n")

      s"""You are an expert software engineer. You have tools for reading, writing, and editing files, executing shell commands, and searching codebases.
         |
         |$envBlock
         |
         |Guidelines:
         |- Read files before editing to understand context
         |- Use exact string matching for edits with sufficient context
         |- Follow existing code patterns and conventions
         |- Handle errors and validate inputs
         |- Use absolute file paths$docsSection""".stripMargin
