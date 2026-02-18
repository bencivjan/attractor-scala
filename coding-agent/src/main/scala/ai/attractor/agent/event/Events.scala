package ai.attractor.agent.event

import cats.effect.IO
import cats.effect.std.Queue
import fs2.Stream

enum EventKind:
  case SessionStart, SessionEnd, UserInput
  case AssistantTextStart, AssistantTextDelta, AssistantTextEnd
  case ToolCallStart, ToolCallOutputDelta, ToolCallEnd
  case SteeringInjected, TurnLimit, LoopDetection
  case Error, Warning

case class SessionEvent(
    kind: EventKind,
    timestamp: java.time.Instant,
    sessionId: String,
    data: Map[String, Any] = Map.empty
)

object SessionEvent:
  def apply(kind: EventKind, sessionId: String): SessionEvent =
    SessionEvent(kind, java.time.Instant.now(), sessionId)

  def apply(kind: EventKind, sessionId: String, data: Map[String, Any]): SessionEvent =
    SessionEvent(kind, java.time.Instant.now(), sessionId, data)

// EventEmitter trait for delivering events to host
trait EventEmitter[F[_]]:
  def emit(event: SessionEvent): F[Unit]
  def events: Stream[F, SessionEvent]

class QueueEventEmitter(queue: Queue[IO, SessionEvent]) extends EventEmitter[IO]:
  override def emit(event: SessionEvent): IO[Unit] =
    queue.offer(event)

  override def events: Stream[IO, SessionEvent] =
    Stream.fromQueueUnterminated(queue)

object QueueEventEmitter:
  def create: IO[QueueEventEmitter] =
    Queue.unbounded[IO, SessionEvent].map(new QueueEventEmitter(_))
