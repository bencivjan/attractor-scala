package ai.attractor.llm.model

import io.circe.Json
import scala.concurrent.duration.FiniteDuration

// ---------------------------------------------------------------------------
// Base SDK error
// ---------------------------------------------------------------------------

abstract class SDKError(
    message: String,
    val retryable: Boolean = false,
    cause: Option[Throwable] = None
) extends Exception(message, cause.orNull)

// ---------------------------------------------------------------------------
// Provider error - base for all provider-originated errors
// ---------------------------------------------------------------------------

class ProviderError(
    message: String,
    val provider: String,
    val statusCode: Option[Int] = None,
    val errorCode: Option[String] = None,
    override val retryable: Boolean = false,
    val retryAfter: Option[FiniteDuration] = None,
    val raw: Option[Json] = None,
    cause: Option[Throwable] = None
) extends SDKError(message, retryable, cause)

// ---------------------------------------------------------------------------
// Specific error subtypes
// ---------------------------------------------------------------------------

final class AuthenticationError(
    message: String,
    provider: String,
    statusCode: Option[Int] = Some(401),
    errorCode: Option[String] = None,
    raw: Option[Json] = None,
    cause: Option[Throwable] = None
) extends ProviderError(message, provider, statusCode, errorCode, retryable = false, raw = raw, cause = cause)

final class AccessDeniedError(
    message: String,
    provider: String,
    statusCode: Option[Int] = Some(403),
    errorCode: Option[String] = None,
    raw: Option[Json] = None,
    cause: Option[Throwable] = None
) extends ProviderError(message, provider, statusCode, errorCode, retryable = false, raw = raw, cause = cause)

final class NotFoundError(
    message: String,
    provider: String,
    statusCode: Option[Int] = Some(404),
    errorCode: Option[String] = None,
    raw: Option[Json] = None,
    cause: Option[Throwable] = None
) extends ProviderError(message, provider, statusCode, errorCode, retryable = false, raw = raw, cause = cause)

final class InvalidRequestError(
    message: String,
    provider: String,
    statusCode: Option[Int] = Some(400),
    errorCode: Option[String] = None,
    raw: Option[Json] = None,
    cause: Option[Throwable] = None
) extends ProviderError(message, provider, statusCode, errorCode, retryable = false, raw = raw, cause = cause)

final class RateLimitError(
    message: String,
    provider: String,
    statusCode: Option[Int] = Some(429),
    errorCode: Option[String] = None,
    retryAfter: Option[FiniteDuration] = None,
    raw: Option[Json] = None,
    cause: Option[Throwable] = None
) extends ProviderError(message, provider, statusCode, errorCode, retryable = true, retryAfter = retryAfter, raw = raw, cause = cause)

final class ServerError(
    message: String,
    provider: String,
    statusCode: Option[Int] = Some(500),
    errorCode: Option[String] = None,
    raw: Option[Json] = None,
    cause: Option[Throwable] = None
) extends ProviderError(message, provider, statusCode, errorCode, retryable = true, raw = raw, cause = cause)

final class ContentFilterError(
    message: String,
    provider: String,
    statusCode: Option[Int] = None,
    errorCode: Option[String] = None,
    raw: Option[Json] = None,
    cause: Option[Throwable] = None
) extends ProviderError(message, provider, statusCode, errorCode, retryable = false, raw = raw, cause = cause)

final class ContextLengthError(
    message: String,
    provider: String,
    statusCode: Option[Int] = Some(400),
    errorCode: Option[String] = None,
    raw: Option[Json] = None,
    cause: Option[Throwable] = None
) extends ProviderError(message, provider, statusCode, errorCode, retryable = false, raw = raw, cause = cause)

final class QuotaExceededError(
    message: String,
    provider: String,
    statusCode: Option[Int] = Some(429),
    errorCode: Option[String] = None,
    raw: Option[Json] = None,
    cause: Option[Throwable] = None
) extends ProviderError(message, provider, statusCode, errorCode, retryable = false, raw = raw, cause = cause)

final class RequestTimeoutError(
    message: String,
    provider: String,
    statusCode: Option[Int] = Some(408),
    errorCode: Option[String] = None,
    raw: Option[Json] = None,
    cause: Option[Throwable] = None
) extends ProviderError(message, provider, statusCode, errorCode, retryable = true, raw = raw, cause = cause)

final class AbortError(
    message: String,
    provider: String,
    cause: Option[Throwable] = None
) extends ProviderError(message, provider, retryable = false, cause = cause)

final class NetworkError(
    message: String,
    provider: String,
    cause: Option[Throwable] = None
) extends ProviderError(message, provider, retryable = true, cause = cause)

final class StreamError(
    message: String,
    provider: String,
    errorCode: Option[String] = None,
    raw: Option[Json] = None,
    cause: Option[Throwable] = None
) extends ProviderError(message, provider, errorCode = errorCode, retryable = false, raw = raw, cause = cause)

final class InvalidToolCallError(
    message: String,
    provider: String,
    errorCode: Option[String] = None,
    raw: Option[Json] = None,
    cause: Option[Throwable] = None
) extends ProviderError(message, provider, errorCode = errorCode, retryable = false, raw = raw, cause = cause)

final class NoObjectGeneratedError(
    message: String,
    provider: String,
    raw: Option[Json] = None,
    cause: Option[Throwable] = None
) extends ProviderError(message, provider, retryable = false, raw = raw, cause = cause)

// ---------------------------------------------------------------------------
// Configuration error (not provider-specific)
// ---------------------------------------------------------------------------

final class ConfigurationError(
    message: String,
    cause: Option[Throwable] = None
) extends SDKError(message, retryable = false, cause = cause)
