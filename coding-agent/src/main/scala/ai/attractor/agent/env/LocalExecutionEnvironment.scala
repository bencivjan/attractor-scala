package ai.attractor.agent.env

import cats.effect.IO
import cats.syntax.all.*

import java.io.{BufferedReader, InputStreamReader}
import java.nio.file.{Files, Path, Paths, FileVisitOption}
import java.nio.charset.StandardCharsets
import java.util.concurrent.TimeUnit
import java.util.regex.Pattern
import java.util.stream.Collectors
import scala.jdk.CollectionConverters.*
import scala.util.Try

class LocalExecutionEnvironment(
    val workingDirectory: String
) extends ExecutionEnvironment[IO]:

  val platform: String = System.getProperty("os.name", "unknown")
  val osVersion: String = System.getProperty("os.version", "unknown")

  // Environment variable patterns to filter out for security
  private val sensitivePatterns: List[String] =
    List("_API_KEY", "_SECRET", "_TOKEN", "_PASSWORD", "_CREDENTIAL")

  private def isSensitiveEnvVar(name: String): Boolean =
    val upper = name.toUpperCase
    sensitivePatterns.exists(pattern => upper.endsWith(pattern))

  private def filteredEnvironment: Map[String, String] =
    System.getenv().asScala.toMap.filterNot { case (k, _) => isSensitiveEnvVar(k) }

  override def initialize: IO[Unit] = IO.unit

  override def cleanup: IO[Unit] = IO.unit

  override def readFile(path: String, offset: Option[Int], limit: Option[Int]): IO[String] =
    IO.blocking:
      val resolved = resolvePath(path)
      val allLines = Files.readAllLines(resolved, StandardCharsets.UTF_8).asScala.toVector
      val startIdx = offset.getOrElse(0).max(0)
      val endIdx = limit.fold(allLines.length)(l => (startIdx + l).min(allLines.length))
      val selectedLines = allLines.slice(startIdx, endIdx)

      // Format with line numbers: "NNN | content"
      selectedLines.zipWithIndex.map { case (line, idx) =>
        val lineNum = startIdx + idx + 1
        f"$lineNum%4d | $line"
      }.mkString("\n")

  override def writeFile(path: String, content: String): IO[Unit] =
    IO.blocking:
      val resolved = resolvePath(path)
      val parent = resolved.getParent
      if parent != null && !Files.exists(parent) then
        Files.createDirectories(parent)
      Files.writeString(resolved, content, StandardCharsets.UTF_8)

  override def fileExists(path: String): IO[Boolean] =
    IO.blocking(Files.exists(resolvePath(path)))

  override def listDirectory(path: String, depth: Int): IO[List[DirEntry]] =
    IO.blocking:
      val resolved = resolvePath(path)
      val stream = Files.walk(resolved, depth.max(1))
      try
        stream
          .filter(p => p != resolved)
          .map { p =>
            val isDirectory = Files.isDirectory(p)
            val size = if isDirectory then None else Try(Files.size(p)).toOption
            val relativeName = resolved.relativize(p).toString
            DirEntry(relativeName, isDirectory, size)
          }
          .collect(Collectors.toList[DirEntry])
          .asScala
          .toList
      finally
        stream.close()

  override def execCommand(
      command: String,
      timeoutMs: Int,
      workingDir: Option[String],
      envVars: Option[Map[String, String]]
  ): IO[ExecResult] =
    IO.blocking:
      val startTime = System.currentTimeMillis()
      val dir = workingDir.map(d => resolvePath(d).toFile).getOrElse(Path.of(workingDirectory).toFile)

      val isWindows = platform.toLowerCase.contains("win")
      val cmdParts = if isWindows then Array("cmd", "/c", command) else Array("bash", "-c", command)

      val pb = new ProcessBuilder(cmdParts*)
      pb.directory(dir)

      // Set filtered environment with optional overrides
      val processEnv = pb.environment()
      processEnv.clear()
      filteredEnvironment.foreach { case (k, v) => processEnv.put(k, v) }
      envVars.foreach(_.foreach { case (k, v) => processEnv.put(k, v) })

      val process = pb.start()

      // Read stdout and stderr in separate threads to avoid deadlocks
      val stdoutFuture = scala.concurrent.Future {
        val reader = new BufferedReader(new InputStreamReader(process.getInputStream, StandardCharsets.UTF_8))
        try
          val sb = new StringBuilder
          var line = reader.readLine()
          while line != null do
            if sb.nonEmpty then sb.append('\n')
            sb.append(line)
            line = reader.readLine()
          sb.toString
        finally reader.close()
      }(scala.concurrent.ExecutionContext.global)

      val stderrFuture = scala.concurrent.Future {
        val reader = new BufferedReader(new InputStreamReader(process.getErrorStream, StandardCharsets.UTF_8))
        try
          val sb = new StringBuilder
          var line = reader.readLine()
          while line != null do
            if sb.nonEmpty then sb.append('\n')
            sb.append(line)
            line = reader.readLine()
          sb.toString
        finally reader.close()
      }(scala.concurrent.ExecutionContext.global)

      val finished = process.waitFor(timeoutMs.toLong, TimeUnit.MILLISECONDS)
      val timedOut = !finished

      if timedOut then
        // SIGTERM first
        destroyProcessTree(process)
        // Wait 2 seconds for graceful shutdown
        val graceful = process.waitFor(2000L, TimeUnit.MILLISECONDS)
        if !graceful then
          // SIGKILL
          process.destroyForcibly()
          process.waitFor(1000L, TimeUnit.MILLISECONDS)

      import scala.concurrent.Await
      import scala.concurrent.duration.*
      val stdout = Try(Await.result(stdoutFuture, 5.seconds)).getOrElse("")
      val stderr = Try(Await.result(stderrFuture, 5.seconds)).getOrElse("")
      val exitCode = if timedOut then -1 else process.exitValue()
      val durationMs = System.currentTimeMillis() - startTime

      ExecResult(stdout, stderr, exitCode, timedOut, durationMs)

  private def destroyProcessTree(process: Process): Unit =
    // Use process.descendants() on JDK 9+ to kill the whole tree
    Try {
      process.descendants().forEach(ph => ph.destroy())
    }
    process.destroy()

  override def grep(pattern: String, path: String, options: GrepOptions): IO[String] =
    hasRipgrep.flatMap { hasRg =>
      if hasRg then ripgrepSearch(pattern, path, options)
      else javaRegexGrep(pattern, path, options)
    }

  private val hasRipgrepCached: IO[Boolean] = IO.blocking {
    Try {
      val pb = new ProcessBuilder("rg", "--version")
      val p = pb.start()
      p.waitFor(3000L, TimeUnit.MILLISECONDS)
      p.exitValue() == 0
    }.getOrElse(false)
  }.memoize.flatten

  private def hasRipgrep: IO[Boolean] = hasRipgrepCached

  private def ripgrepSearch(pattern: String, path: String, options: GrepOptions): IO[String] =
    IO.blocking:
      val resolved = resolvePath(path)
      val args = scala.collection.mutable.ListBuffer[String]("rg", "--no-heading", "--line-number")

      if options.caseInsensitive then args += "-i"
      args ++= List("-m", options.maxResults.toString)
      options.globFilter.foreach(g => args ++= List("--glob", g))
      args += pattern
      args += resolved.toString

      val pb = new ProcessBuilder(args.toList.asJava)
      pb.redirectErrorStream(true)
      val process = pb.start()

      val reader = new BufferedReader(new InputStreamReader(process.getInputStream, StandardCharsets.UTF_8))
      try
        val sb = new StringBuilder
        var line = reader.readLine()
        while line != null do
          if sb.nonEmpty then sb.append('\n')
          sb.append(line)
          line = reader.readLine()
        process.waitFor(30000L, TimeUnit.MILLISECONDS)
        sb.toString
      finally reader.close()

  private def javaRegexGrep(pattern: String, path: String, options: GrepOptions): IO[String] =
    IO.blocking:
      val resolved = resolvePath(path)
      val flags = if options.caseInsensitive then Pattern.CASE_INSENSITIVE else 0
      val compiledPattern = Pattern.compile(pattern, flags)
      val results = scala.collection.mutable.ListBuffer[String]()
      var count = 0

      def searchFile(file: Path): Unit =
        if count >= options.maxResults then return
        if !Files.isRegularFile(file) then return
        // Check glob filter
        val matchesGlob = options.globFilter.forall { globStr =>
          val matcher = file.getFileSystem.getPathMatcher(s"glob:$globStr")
          matcher.matches(file.getFileName)
        }
        if !matchesGlob then return

        Try {
          val lines = Files.readAllLines(file, StandardCharsets.UTF_8).asScala
          lines.zipWithIndex.foreach { case (line, idx) =>
            if count < options.maxResults && compiledPattern.matcher(line).find() then
              val relPath = resolved.relativize(file)
              results += s"$relPath:${idx + 1}:$line"
              count += 1
          }
        }

      if Files.isDirectory(resolved) then
        val stream = Files.walk(resolved)
        try
          stream.forEach(p => searchFile(p))
        finally
          stream.close()
      else
        searchFile(resolved)

      results.mkString("\n")

  override def glob(pattern: String, path: String): IO[List[String]] =
    IO.blocking:
      val resolved = resolvePath(path)
      val matcher = resolved.getFileSystem.getPathMatcher(s"glob:$pattern")
      val results = scala.collection.mutable.ListBuffer[String]()

      val stream = Files.walk(resolved)
      try
        stream.forEach { p =>
          val relative = resolved.relativize(p)
          if matcher.matches(relative) then
            results += relative.toString
        }
      finally
        stream.close()

      results.toList.sorted

  private def resolvePath(path: String): Path =
    val p = Paths.get(path)
    if p.isAbsolute then p
    else Paths.get(workingDirectory).resolve(p)

object LocalExecutionEnvironment:
  def create(workingDirectory: String): IO[LocalExecutionEnvironment] =
    IO.pure(new LocalExecutionEnvironment(workingDirectory))
