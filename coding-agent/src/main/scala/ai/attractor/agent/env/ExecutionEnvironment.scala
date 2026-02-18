package ai.attractor.agent.env

case class ExecResult(
    stdout: String,
    stderr: String,
    exitCode: Int,
    timedOut: Boolean,
    durationMs: Long
)

case class DirEntry(
    name: String,
    isDir: Boolean,
    size: Option[Long]
)

case class GrepOptions(
    caseInsensitive: Boolean = false,
    maxResults: Int = 100,
    globFilter: Option[String] = None
)

trait ExecutionEnvironment[F[_]]:
  def readFile(path: String, offset: Option[Int] = None, limit: Option[Int] = None): F[String]
  def writeFile(path: String, content: String): F[Unit]
  def fileExists(path: String): F[Boolean]
  def listDirectory(path: String, depth: Int = 1): F[List[DirEntry]]
  def execCommand(
      command: String,
      timeoutMs: Int,
      workingDir: Option[String] = None,
      envVars: Option[Map[String, String]] = None
  ): F[ExecResult]
  def grep(pattern: String, path: String, options: GrepOptions = GrepOptions()): F[String]
  def glob(pattern: String, path: String): F[List[String]]
  def initialize: F[Unit]
  def cleanup: F[Unit]
  def workingDirectory: String
  def platform: String
  def osVersion: String
