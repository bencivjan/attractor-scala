package ai.attractor.agent.tool

import ai.attractor.llm.model.ToolDefinition
import ai.attractor.agent.env.{ExecutionEnvironment, GrepOptions}
import cats.effect.IO
import io.circe.{Json, JsonObject}
import io.circe.syntax.*

object CoreTools:

  // ---------------------------------------------------------------------------
  // readFile
  // ---------------------------------------------------------------------------
  val readFile: RegisteredTool[IO] = RegisteredTool(
    definition = ToolDefinition(
      name = "read_file",
      description = "Reads a file from the filesystem. Returns content with line numbers in 'NNN | content' format. Supports optional offset and limit for reading specific line ranges.",
      parameters = Json.obj(
        "type" -> "object".asJson,
        "properties" -> Json.obj(
          "file_path" -> Json.obj(
            "type" -> "string".asJson,
            "description" -> "The absolute path to the file to read".asJson
          ),
          "offset" -> Json.obj(
            "type" -> "integer".asJson,
            "description" -> "Line number to start reading from (0-indexed). Defaults to 0.".asJson
          ),
          "limit" -> Json.obj(
            "type" -> "integer".asJson,
            "description" -> "Maximum number of lines to read. Reads all lines if not specified.".asJson
          )
        ),
        "required" -> List("file_path").asJson
      )
    ),
    executor = (args, env) =>
      val filePath = args.hcursor.get[String]("file_path").getOrElse("")
      val offset = args.hcursor.get[Int]("offset").toOption
      val limit = args.hcursor.get[Int]("limit").toOption
      env.readFile(filePath, offset, limit)
  )

  // ---------------------------------------------------------------------------
  // writeFile
  // ---------------------------------------------------------------------------
  val writeFile: RegisteredTool[IO] = RegisteredTool(
    definition = ToolDefinition(
      name = "write_file",
      description = "Writes content to a file, creating parent directories as needed. Overwrites existing file content.",
      parameters = Json.obj(
        "type" -> "object".asJson,
        "properties" -> Json.obj(
          "file_path" -> Json.obj(
            "type" -> "string".asJson,
            "description" -> "The absolute path to the file to write".asJson
          ),
          "content" -> Json.obj(
            "type" -> "string".asJson,
            "description" -> "The content to write to the file".asJson
          )
        ),
        "required" -> List("file_path", "content").asJson
      )
    ),
    executor = (args, env) =>
      val filePath = args.hcursor.get[String]("file_path").getOrElse("")
      val content = args.hcursor.get[String]("content").getOrElse("")
      env.writeFile(filePath, content).map(_ => s"Successfully wrote to $filePath")
  )

  // ---------------------------------------------------------------------------
  // editFile
  // ---------------------------------------------------------------------------
  val editFile: RegisteredTool[IO] = RegisteredTool(
    definition = ToolDefinition(
      name = "edit_file",
      description = "Performs exact string replacement in a file. The old_string must match exactly (including whitespace and indentation). Use replace_all to replace every occurrence.",
      parameters = Json.obj(
        "type" -> "object".asJson,
        "properties" -> Json.obj(
          "file_path" -> Json.obj(
            "type" -> "string".asJson,
            "description" -> "The absolute path to the file to edit".asJson
          ),
          "old_string" -> Json.obj(
            "type" -> "string".asJson,
            "description" -> "The exact text to find and replace".asJson
          ),
          "new_string" -> Json.obj(
            "type" -> "string".asJson,
            "description" -> "The replacement text".asJson
          ),
          "replace_all" -> Json.obj(
            "type" -> "boolean".asJson,
            "description" -> "If true, replace all occurrences. Default is false (replace first only).".asJson
          )
        ),
        "required" -> List("file_path", "old_string", "new_string").asJson
      )
    ),
    executor = (args, env) =>
      val filePath = args.hcursor.get[String]("file_path").getOrElse("")
      val oldString = args.hcursor.get[String]("old_string").getOrElse("")
      val newString = args.hcursor.get[String]("new_string").getOrElse("")
      val replaceAll = args.hcursor.get[Boolean]("replace_all").getOrElse(false)

      for
        content <- env.readFile(filePath)
        // readFile returns line-numbered content; we need raw content for editing
        rawContent <- IO.blocking {
          java.nio.file.Files.readString(
            java.nio.file.Paths.get(filePath),
            java.nio.charset.StandardCharsets.UTF_8
          )
        }
        _ <-
          if !rawContent.contains(oldString) then
            IO.raiseError(new RuntimeException(
              s"old_string not found in $filePath. Make sure it matches exactly including whitespace."
            ))
          else IO.unit
        occurrences = rawContent.split(java.util.regex.Pattern.quote(oldString), -1).length - 1
        _ <-
          if !replaceAll && occurrences > 1 then
            IO.raiseError(new RuntimeException(
              s"old_string found $occurrences times in $filePath. Use replace_all=true to replace all, or provide more context to make it unique."
            ))
          else IO.unit
        newContent =
          if replaceAll then rawContent.replace(oldString, newString)
          else
            val idx = rawContent.indexOf(oldString)
            rawContent.substring(0, idx) + newString + rawContent.substring(idx + oldString.length)
        _ <- env.writeFile(filePath, newContent)
      yield
        val count = if replaceAll then occurrences else 1
        s"Successfully replaced $count occurrence(s) in $filePath"
  )

  // ---------------------------------------------------------------------------
  // shell
  // ---------------------------------------------------------------------------
  val shell: RegisteredTool[IO] = RegisteredTool(
    definition = ToolDefinition(
      name = "shell",
      description = "Executes a shell command and returns stdout, stderr, and exit code. Commands run in bash on Unix or cmd on Windows.",
      parameters = Json.obj(
        "type" -> "object".asJson,
        "properties" -> Json.obj(
          "command" -> Json.obj(
            "type" -> "string".asJson,
            "description" -> "The command to execute".asJson
          ),
          "timeout_ms" -> Json.obj(
            "type" -> "integer".asJson,
            "description" -> "Timeout in milliseconds. Default is 10000.".asJson
          ),
          "working_dir" -> Json.obj(
            "type" -> "string".asJson,
            "description" -> "Working directory for the command. Defaults to session working directory.".asJson
          )
        ),
        "required" -> List("command").asJson
      )
    ),
    executor = (args, env) =>
      val command = args.hcursor.get[String]("command").getOrElse("")
      val timeoutMs = args.hcursor.get[Int]("timeout_ms").getOrElse(10000)
      val workingDir = args.hcursor.get[String]("working_dir").toOption

      env.execCommand(command, timeoutMs, workingDir).map { result =>
        val sb = new StringBuilder
        if result.stdout.nonEmpty then
          sb.append(result.stdout)
        if result.stderr.nonEmpty then
          if sb.nonEmpty then sb.append('\n')
          sb.append("STDERR:\n")
          sb.append(result.stderr)
        if result.timedOut then
          if sb.nonEmpty then sb.append('\n')
          sb.append(s"Command timed out after ${timeoutMs}ms")
        if result.exitCode != 0 && !result.timedOut then
          if sb.nonEmpty then sb.append('\n')
          sb.append(s"Exit code: ${result.exitCode}")
        sb.toString
      }
  )

  // ---------------------------------------------------------------------------
  // grep
  // ---------------------------------------------------------------------------
  val grep: RegisteredTool[IO] = RegisteredTool(
    definition = ToolDefinition(
      name = "grep",
      description = "Searches for a regex pattern in files. Uses ripgrep if available, falls back to Java regex. Returns matching lines with file paths and line numbers.",
      parameters = Json.obj(
        "type" -> "object".asJson,
        "properties" -> Json.obj(
          "pattern" -> Json.obj(
            "type" -> "string".asJson,
            "description" -> "The regex pattern to search for".asJson
          ),
          "path" -> Json.obj(
            "type" -> "string".asJson,
            "description" -> "File or directory path to search in".asJson
          ),
          "case_insensitive" -> Json.obj(
            "type" -> "boolean".asJson,
            "description" -> "Enable case-insensitive search. Default is false.".asJson
          ),
          "max_results" -> Json.obj(
            "type" -> "integer".asJson,
            "description" -> "Maximum number of matching lines to return. Default is 100.".asJson
          ),
          "glob_filter" -> Json.obj(
            "type" -> "string".asJson,
            "description" -> "Glob pattern to filter files (e.g. '*.scala')".asJson
          )
        ),
        "required" -> List("pattern", "path").asJson
      )
    ),
    executor = (args, env) =>
      val pattern = args.hcursor.get[String]("pattern").getOrElse("")
      val path = args.hcursor.get[String]("path").getOrElse("")
      val caseInsensitive = args.hcursor.get[Boolean]("case_insensitive").getOrElse(false)
      val maxResults = args.hcursor.get[Int]("max_results").getOrElse(100)
      val globFilter = args.hcursor.get[String]("glob_filter").toOption

      env.grep(pattern, path, GrepOptions(caseInsensitive, maxResults, globFilter))
  )

  // ---------------------------------------------------------------------------
  // glob
  // ---------------------------------------------------------------------------
  val glob: RegisteredTool[IO] = RegisteredTool(
    definition = ToolDefinition(
      name = "glob",
      description = "Finds files matching a glob pattern. Returns a list of matching file paths relative to the search directory.",
      parameters = Json.obj(
        "type" -> "object".asJson,
        "properties" -> Json.obj(
          "pattern" -> Json.obj(
            "type" -> "string".asJson,
            "description" -> "The glob pattern to match (e.g. '**/*.scala')".asJson
          ),
          "path" -> Json.obj(
            "type" -> "string".asJson,
            "description" -> "Base directory to search in".asJson
          )
        ),
        "required" -> List("pattern", "path").asJson
      )
    ),
    executor = (args, env) =>
      val pattern = args.hcursor.get[String]("pattern").getOrElse("")
      val path = args.hcursor.get[String]("path").getOrElse("")
      env.glob(pattern, path).map(_.mkString("\n"))
  )

  // ---------------------------------------------------------------------------
  // All core tools
  // ---------------------------------------------------------------------------
  val all: List[RegisteredTool[IO]] =
    List(readFile, writeFile, editFile, shell, grep, glob)

  /** Core tools excluding edit_file (for providers like OpenAI that use apply_patch) */
  val withoutEdit: List[RegisteredTool[IO]] =
    all.filterNot(_.definition.name == "edit_file")
