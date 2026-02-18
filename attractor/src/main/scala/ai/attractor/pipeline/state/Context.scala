package ai.attractor.pipeline.state

import cats.effect.{IO, Ref}

// ---------------------------------------------------------------------------
// Thread-safe pipeline execution context backed by cats-effect Ref.
//
// Context holds arbitrary key-value pairs used to pass data between pipeline
// stages, along with an append-only execution log.  All mutations go through
// IO to guarantee safe concurrent access when stages run in parallel.
// ---------------------------------------------------------------------------

class Context private (
    private val values: Ref[IO, Map[String, Any]],
    private val logEntries: Ref[IO, List[String]]
):

  /** Set a single key-value pair in the context. */
  def set(key: String, value: Any): IO[Unit] =
    values.update(_ + (key -> value))

  /** Get a value by key, returning a default if absent. */
  def get(key: String, default: Any = null): IO[Any] =
    values.get.map(_.getOrElse(key, default))

  /** Get a value as a String, returning a default if absent or non-string. */
  def getString(key: String, default: String = ""): IO[String] =
    values.get.map: m =>
      m.get(key) match
        case Some(s: String) => s
        case Some(other)     => other.toString
        case None            => default

  /** Append a log entry to the execution log. */
  def appendLog(entry: String): IO[Unit] =
    logEntries.update(_ :+ entry)

  /** Get the current log entries. */
  def getLog: IO[List[String]] =
    logEntries.get

  /** Take an immutable snapshot of all context values. */
  def snapshot: IO[Map[String, Any]] =
    values.get

  /** Create a deep copy with independent Refs. */
  override def clone: IO[Context] =
    for
      currentValues <- values.get
      currentLogs   <- logEntries.get
      newValues     <- Ref[IO].of(currentValues)
      newLogs       <- Ref[IO].of(currentLogs)
    yield new Context(newValues, newLogs)

  /** Apply a batch of updates atomically. */
  def applyUpdates(updates: Map[String, Any]): IO[Unit] =
    values.update(_ ++ updates)

object Context:
  /** Create an empty context. */
  def empty: IO[Context] =
    for
      values     <- Ref[IO].of(Map.empty[String, Any])
      logEntries <- Ref[IO].of(List.empty[String])
    yield new Context(values, logEntries)

  /** Create a context pre-populated with the given values. */
  def of(initial: Map[String, Any]): IO[Context] =
    for
      values     <- Ref[IO].of(initial)
      logEntries <- Ref[IO].of(List.empty[String])
    yield new Context(values, logEntries)
