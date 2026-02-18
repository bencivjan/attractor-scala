package ai.attractor.llm.util

import cats.effect.Concurrent
import fs2.{Pipe, Pull, Stream}

/** A parsed Server-Sent Event. */
final case class SSEEvent(
  eventType: Option[String],
  data: String,
  id: Option[String] = None,
  retry: Option[Int] = None
)

/** SSE (Server-Sent Events) parser that transforms a raw byte stream into
  * a stream of [[SSEEvent]] values.
  *
  * Follows the W3C EventSource specification:
  *   - Lines starting with ':' are comments (ignored)
  *   - Empty lines delimit events
  *   - Fields: event, data, id, retry
  *   - Multiple data lines are concatenated with newlines
  */
object SSEParser:

  /** fs2 Pipe that decodes a byte stream into SSE events.
    *
    * The pipe first decodes bytes into UTF-8 text, splits on line boundaries,
    * then accumulates field values until a blank-line delimiter is encountered.
    */
  def parse[F[_]: Concurrent]: Pipe[F, Byte, SSEEvent] =
    (bytes: Stream[F, Byte]) =>
      bytes
        .through(fs2.text.utf8.decode)
        .through(fs2.text.lines)
        .through(accumulateEvents)

  // -- internal ---------------------------------------------------------------

  private case class EventBuilder(
    eventType: Option[String] = None,
    dataLines: List[String] = Nil,
    id: Option[String] = None,
    retry: Option[Int] = None
  ):
    def isEmpty: Boolean = dataLines.isEmpty && eventType.isEmpty

    def build: Option[SSEEvent] =
      if dataLines.isEmpty then None
      else
        Some(SSEEvent(
          eventType = eventType,
          data = dataLines.reverse.mkString("\n"),
          id = id,
          retry = retry
        ))

  private def accumulateEvents[F[_]]: Pipe[F, String, SSEEvent] =
    (lines: Stream[F, String]) =>
      lines
        .mapAccumulate(EventBuilder()): (builder, line) =>
          if line.isEmpty then
            // Blank line: emit the accumulated event and reset.
            (EventBuilder(), builder.build)
          else if line.startsWith(":") then
            // Comment line: ignore.
            (builder, None)
          else
            val (field, value) = line.indexOf(':') match
              case -1  => (line, "")
              case idx =>
                val v = line.substring(idx + 1)
                // Strip a single leading space from the value per spec.
                val trimmed = if v.startsWith(" ") then v.substring(1) else v
                (line.substring(0, idx), trimmed)

            val updated = field match
              case "event" => builder.copy(eventType = Some(value))
              case "data"  => builder.copy(dataLines = value :: builder.dataLines)
              case "id"    => builder.copy(id = if value.contains('\u0000') then builder.id else Some(value))
              case "retry" => builder.copy(retry = value.toIntOption.orElse(builder.retry))
              case _       => builder // Unknown fields are ignored per spec.
            (updated, None)
        .map(_._2)
        .unNone
