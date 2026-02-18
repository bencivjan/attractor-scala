package ai.attractor.llm.middleware

import ai.attractor.llm.model.*
import cats.effect.{IO, Ref}
import cats.effect.std.Semaphore

import scala.concurrent.duration.*

/** A middleware is a function that wraps a request handler, allowing
  * cross-cutting concerns (logging, cost tracking, rate limiting, etc.)
  * to be layered transparently.
  *
  * {{{
  *   type Middleware = (Request, Request => IO[Response]) => IO[Response]
  * }}}
  *
  * Middleware can be composed left-to-right:
  * {{{
  *   val stack = List(LoggingMiddleware(logger), CostTrackingMiddleware(tracker))
  *   val handler = stack.foldRight(baseHandler) { (mw, next) => req => mw(req, next) }
  * }}}
  */
type Middleware = (Request, Request => IO[Response]) => IO[Response]

// ---------------------------------------------------------------------------
// Logging
// ---------------------------------------------------------------------------

/** Logs request model/message-count before the call and response
  * finish-reason/usage after the call.
  */
object LoggingMiddleware:

  def apply(logger: String => IO[Unit]): Middleware =
    (request, next) =>
      for
        _        <- logger(s"[LLM] request model=${request.model} messages=${request.messages.size} tools=${request.tools.size}")
        response <- next(request)
        _        <- logger(
                      s"[LLM] response id=${response.id} model=${response.model} " +
                      s"finish=${response.finishReason.reason} " +
                      s"tokens_in=${response.usage.inputTokens} tokens_out=${response.usage.outputTokens}"
                    )
      yield response

// ---------------------------------------------------------------------------
// Cost tracking
// ---------------------------------------------------------------------------

/** Passes the response Usage to a caller-supplied tracker after every
  * successful completion.
  */
object CostTrackingMiddleware:

  def apply(tracker: Usage => IO[Unit]): Middleware =
    (request, next) =>
      next(request).flatTap(response => tracker(response.usage))

// ---------------------------------------------------------------------------
// Rate limiting
// ---------------------------------------------------------------------------

/** Simple token-bucket rate limiter that enforces a maximum number of
  * requests per minute.
  *
  * The returned `IO[Middleware]` must be evaluated once to allocate the
  * internal state (a semaphore and a timestamp reference). The resulting
  * Middleware instance is then safe for concurrent use.
  *
  * Implementation: a fixed-window approach that resets the counter every
  * 60 seconds. This is simpler than a sliding window but sufficient for
  * most SDK use cases where the goal is to stay below provider rate limits.
  */
object RateLimitMiddleware:

  def apply(requestsPerMinute: Int): IO[Middleware] =
    for
      semaphore  <- Semaphore[IO](requestsPerMinute.toLong)
      windowRef  <- Ref.of[IO, Long](System.currentTimeMillis())
      countRef   <- Ref.of[IO, Int](0)
    yield
      (request, next) =>
        for
          now          <- IO(System.currentTimeMillis())
          windowStart  <- windowRef.get
          _            <- IO.whenA(now - windowStart >= 60_000L):
                            windowRef.set(now) *> countRef.set(0) *> refillSemaphore(semaphore, countRef, requestsPerMinute)
          _            <- semaphore.acquire
          _            <- countRef.update(_ + 1)
          response     <- next(request).onError(_ => semaphore.release)
          _            <- semaphore.release
        yield response

  /** Refill the semaphore to match the difference between the rpm cap and
    * the current count (which was just reset to 0).
    */
  private def refillSemaphore(
      semaphore: Semaphore[IO],
      countRef: Ref[IO, Int],
      rpm: Int
  ): IO[Unit] =
    semaphore.count.flatMap: available =>
      val deficit = rpm.toLong - available
      if deficit > 0 then semaphore.releaseN(deficit)
      else IO.unit
