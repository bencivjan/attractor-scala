package ai.attractor.llm.util

import cats.effect.IO
import scala.concurrent.duration.*

import ai.attractor.llm.model.{ProviderError, SDKError}

/** Configuration for exponential-backoff retry with optional jitter. */
final case class RetryPolicy(
  maxRetries: Int = 2,
  baseDelay: FiniteDuration = 1.second,
  maxDelay: FiniteDuration = 60.seconds,
  backoffMultiplier: Double = 2.0,
  jitter: Boolean = true
)

object RetryPolicy:

  val default: RetryPolicy = RetryPolicy()

  /** Execute `action` with retries according to the given `policy`.
    *
    * Retryable errors are determined by inspecting the error hierarchy:
    * [[ProviderError]] carries its own `retryable` flag, while the base
    * [[SDKError]] also exposes `retryable`. Rate-limit and server errors
    * are typically retryable; authentication and bad-request errors are not.
    */
  def retry[A](policy: RetryPolicy)(action: IO[A]): IO[A] =
    def loop(attempt: Int): IO[A] =
      action.handleErrorWith:
        case err: ProviderError if err.retryable && attempt < policy.maxRetries =>
          // Retry-After header overrides computed backoff when present and within maxDelay
          val delay = err.retryAfter match
            case Some(ra) if ra <= policy.maxDelay => ra
            case Some(ra)                          => policy.maxDelay
            case None                              => computeDelay(policy, attempt)
          IO.sleep(delay) *> loop(attempt + 1)
        case err: SDKError if err.retryable && attempt < policy.maxRetries =>
          val delay = computeDelay(policy, attempt)
          IO.sleep(delay) *> loop(attempt + 1)
        case err => IO.raiseError(err)
    loop(0)

  /** Compute the delay for a given attempt using exponential backoff and
    * optional jitter.
    *
    * The raw delay is `baseDelay * backoffMultiplier ^ attempt`, clamped to
    * `maxDelay`. When jitter is enabled the actual delay is drawn uniformly
    * from `[0, rawDelay]` to decorrelate concurrent retriers.
    */
  private def computeDelay(policy: RetryPolicy, attempt: Int): FiniteDuration =
    val rawMs = (policy.baseDelay.toMillis * math.pow(policy.backoffMultiplier, attempt.toDouble)).toLong
    val clampedMs = math.min(rawMs, policy.maxDelay.toMillis)
    val finalMs =
      if policy.jitter then (math.random() * clampedMs).toLong
      else clampedMs
    finalMs.millis
