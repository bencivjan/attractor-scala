package ai.attractor.llm.model

import munit.FunSuite
import scala.concurrent.duration.*

class ErrorsSuite extends FunSuite:

  test("AuthenticationError is not retryable"):
    val err = AuthenticationError("Invalid key", provider = "anthropic")
    assert(!err.retryable)
    assertEquals(err.statusCode, Some(401))

  test("RateLimitError is retryable"):
    val err = RateLimitError("Too many requests", provider = "openai")
    assert(err.retryable)
    assertEquals(err.statusCode, Some(429))

  test("RateLimitError carries retryAfter"):
    val err = RateLimitError(
      "Rate limited",
      provider = "anthropic",
      retryAfter = Some(30.seconds)
    )
    assertEquals(err.retryAfter, Some(30.seconds))

  test("ServerError is retryable"):
    val err = ServerError("Internal error", provider = "gemini", statusCode = Some(500))
    assert(err.retryable)

  test("InvalidRequestError is not retryable"):
    val err = InvalidRequestError("Bad request", provider = "openai")
    assert(!err.retryable)

  test("ContentFilterError is not retryable"):
    val err = ContentFilterError("Content blocked", provider = "gemini")
    assert(!err.retryable)

  test("NetworkError is retryable"):
    val err = NetworkError("Connection refused", provider = "anthropic")
    assert(err.retryable)

  test("ConfigurationError is not retryable"):
    val err = ConfigurationError("No provider configured")
    assert(!err.retryable)

  test("ContextLengthError is not retryable"):
    val err = ContextLengthError("Too many tokens", provider = "anthropic")
    assert(!err.retryable)

  test("RequestTimeoutError is retryable"):
    val err = RequestTimeoutError("Timed out", provider = "openai")
    assert(err.retryable)
