package ai.attractor.llm.util

import munit.CatsEffectSuite
import cats.effect.IO
import scala.concurrent.duration.*

import ai.attractor.llm.model.*

class RetryPolicySuite extends CatsEffectSuite:

  test("retry succeeds on first attempt"):
    var attempts = 0
    val policy = RetryPolicy(maxRetries = 3, baseDelay = 10.millis, jitter = false)
    RetryPolicy.retry(policy):
      IO { attempts += 1; "ok" }
    .map: result =>
      assertEquals(result, "ok")
      assertEquals(attempts, 1)

  test("retry retries on retryable error"):
    var attempts = 0
    val policy = RetryPolicy(maxRetries = 3, baseDelay = 10.millis, jitter = false)
    RetryPolicy.retry(policy):
      IO:
        attempts += 1
        if attempts < 3 then
          throw ServerError("transient", provider = "test")
        "ok"
    .map: result =>
      assertEquals(result, "ok")
      assertEquals(attempts, 3)

  test("retry does not retry non-retryable error"):
    var attempts = 0
    val policy = RetryPolicy(maxRetries = 3, baseDelay = 10.millis, jitter = false)
    RetryPolicy.retry(policy):
      IO:
        attempts += 1
        throw AuthenticationError("bad key", provider = "test")
    .attempt.map: result =>
      assert(result.isLeft)
      assertEquals(attempts, 1)

  test("retry respects maxRetries limit"):
    var attempts = 0
    val policy = RetryPolicy(maxRetries = 2, baseDelay = 10.millis, jitter = false)
    RetryPolicy.retry(policy):
      IO:
        attempts += 1
        throw ServerError("always fails", provider = "test")
    .attempt.map: result =>
      assert(result.isLeft)
      assertEquals(attempts, 3) // initial + 2 retries

  test("retry uses retryAfter from RateLimitError"):
    var attempts = 0
    val policy = RetryPolicy(maxRetries = 1, baseDelay = 10.seconds, maxDelay = 60.seconds, jitter = false)
    // retryAfter should be used instead of computed backoff
    RetryPolicy.retry(policy):
      IO:
        attempts += 1
        if attempts < 2 then
          throw RateLimitError("rate limited", provider = "test", retryAfter = Some(50.millis))
        "ok"
    .map: result =>
      assertEquals(result, "ok")
      assertEquals(attempts, 2)

  test("maxRetries 0 disables retries"):
    var attempts = 0
    val policy = RetryPolicy(maxRetries = 0, baseDelay = 10.millis)
    RetryPolicy.retry(policy):
      IO:
        attempts += 1
        throw ServerError("fails", provider = "test")
    .attempt.map: result =>
      assert(result.isLeft)
      assertEquals(attempts, 1)
