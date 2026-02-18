package ai.attractor.llm.provider

import cats.Applicative
import cats.effect.IO
import fs2.Stream

import ai.attractor.llm.model.*

/** Contract that every LLM provider adapter must implement.
  *
  * Adapters translate between the unified request/response model and the
  * provider's native HTTP API. Each adapter owns its own HTTP client and
  * manages connection lifecycle.
  */
trait ProviderAdapter[F[_]]:

  /** Canonical provider name (e.g. "anthropic", "openai", "gemini"). */
  def name: String

  /** Send a request and block until the model produces a complete response. */
  def complete(request: Request): F[Response]

  /** Send a request and return a stream of incremental events. */
  def stream(request: Request): Stream[F, StreamEvent]

  /** Release resources (HTTP connections, etc.). Called by Client.close(). */
  def close: F[Unit]

  /** Validate configuration on startup. Called by Client on registration. */
  def initialize: F[Unit]

  /** Query whether a particular tool choice mode is supported. */
  def supportsToolChoice(mode: String): Boolean = true

object ProviderAdapter:
  /** Convenience for adapters that use cats-effect IO and have no-op
    * lifecycle methods by default.
    */
  trait IOAdapter extends ProviderAdapter[IO]:
    override def close: IO[Unit] = IO.unit
    override def initialize: IO[Unit] = IO.unit
