package ai.attractor.llm.provider

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.syntax.all.*
import fs2.Stream

import ai.attractor.llm.model.*
import ai.attractor.llm.provider.anthropic.AnthropicAdapter
import ai.attractor.llm.provider.openai.OpenAIAdapter
import ai.attractor.llm.provider.gemini.GeminiAdapter

/** A function that wraps a provider call, enabling cross-cutting concerns
  * such as logging, caching, cost tracking, and rate limiting.
  *
  * Middleware receives the current request and a continuation function. It
  * may modify the request, inspect the response, or perform side effects.
  * Middleware executes in registration order for the request phase and in
  * reverse order for the response phase (standard onion pattern).
  */
type Middleware = (Request, Request => IO[Response]) => IO[Response]

/** Core routing client that dispatches requests to registered provider
  * adapters and applies middleware.
  *
  * The client holds no mutable state between requests. Provider adapters
  * manage their own connection pools and are safe for concurrent use.
  */
final class Client private (
  val providers: Map[String, ProviderAdapter[IO]],
  val defaultProvider: Option[String],
  val middleware: List[Middleware]
):

  /** Send a blocking request, routing to the correct adapter and applying
    * middleware in onion order.
    */
  def complete(request: Request): IO[Response] =
    resolveAdapter(request).flatMap: adapter =>
      val base: Request => IO[Response] = req => adapter.complete(req)
      val chain = middleware.foldRight(base): (mw, next) =>
        req => mw(req, next)
      chain(request)

  /** Send a streaming request, routing to the correct adapter.
    *
    * Middleware is not applied to the streaming path because stream
    * transformations require a different signature. Consumers that need
    * stream-level middleware should wrap the returned Stream directly.
    */
  def stream(request: Request): Stream[IO, StreamEvent] =
    Stream.eval(resolveAdapter(request)).flatMap(_.stream(request))

  /** Close all registered adapters, releasing HTTP connections and other
    * resources.
    */
  def close: IO[Unit] =
    providers.values.toList.traverse_(_.close)

  // -- internals --------------------------------------------------------------

  private def resolveAdapter(request: Request): IO[ProviderAdapter[IO]] =
    val providerName = request.provider.orElse(defaultProvider)
    providerName match
      case Some(name) =>
        providers.get(name) match
          case Some(adapter) => IO.pure(adapter)
          case None =>
            IO.raiseError(ConfigurationError(
              s"Provider '$name' is not registered. Available: ${providers.keys.mkString(", ")}"
            ))
      case None =>
        IO.raiseError(ConfigurationError(
          "No provider specified on the request and no default provider is configured."
        ))

object Client:

  /** Builder for constructing a [[Client]] programmatically. */
  final class Builder private[Client]:
    private var adapters: Map[String, ProviderAdapter[IO]] = Map.empty
    private var default: Option[String] = None
    private var mw: List[Middleware] = Nil

    def withProvider(adapter: ProviderAdapter[IO]): Builder =
      adapters = adapters + (adapter.name -> adapter)
      if default.isEmpty then default = Some(adapter.name)
      this

    def withDefaultProvider(name: String): Builder =
      default = Some(name)
      this

    def withMiddleware(middleware: Middleware): Builder =
      mw = mw :+ middleware
      this

    def build: Client = new Client(adapters, default, mw)

  def builder: Builder = new Builder

  /** Create a [[Client]] from standard environment variables.
    *
    * Reads ANTHROPIC_API_KEY, OPENAI_API_KEY, and GEMINI_API_KEY. Only
    * providers whose keys are present are registered. The first registered
    * provider becomes the default.
    *
    * Returns a cats-effect Resource that manages the lifecycle of the
    * underlying HTTP clients.
    */
  def fromEnv(): Resource[IO, Client] =
    val anthropicKey = Option(System.getenv("ANTHROPIC_API_KEY")).filter(_.nonEmpty)
    val openaiKey    = Option(System.getenv("OPENAI_API_KEY")).filter(_.nonEmpty)
    val geminiKey    = Option(System.getenv("GEMINI_API_KEY"))
      .orElse(Option(System.getenv("GOOGLE_API_KEY")))
      .filter(_.nonEmpty)

    val anthropicBaseUrl = Option(System.getenv("ANTHROPIC_BASE_URL")).filter(_.nonEmpty)
    val openaiBaseUrl    = Option(System.getenv("OPENAI_BASE_URL")).filter(_.nonEmpty)
    val geminiBaseUrl    = Option(System.getenv("GEMINI_BASE_URL")).filter(_.nonEmpty)

    for
      adapters <- buildAdapters(
        anthropicKey, anthropicBaseUrl,
        openaiKey, openaiBaseUrl,
        geminiKey, geminiBaseUrl
      )
      _ <- Resource.eval(
        if adapters.isEmpty then
          IO.raiseError(ConfigurationError(
            "No API keys found in environment. Set at least one of: " +
              "ANTHROPIC_API_KEY, OPENAI_API_KEY, GEMINI_API_KEY"
          ))
        else IO.unit
      )
    yield
      val b = builder
      adapters.foreach(b.withProvider)
      b.build

  private def buildAdapters(
    anthropicKey: Option[String], anthropicBaseUrl: Option[String],
    openaiKey: Option[String], openaiBaseUrl: Option[String],
    geminiKey: Option[String], geminiBaseUrl: Option[String]
  ): Resource[IO, List[ProviderAdapter[IO]]] =
    val anthropicR: Resource[IO, List[ProviderAdapter[IO]]] =
      anthropicKey match
        case Some(key) =>
          AnthropicAdapter.resource(key, anthropicBaseUrl).map(a => List(a))
        case None => Resource.pure(Nil)

    val openaiR: Resource[IO, List[ProviderAdapter[IO]]] =
      openaiKey match
        case Some(key) =>
          OpenAIAdapter.resource(key, openaiBaseUrl).map(a => List(a))
        case None => Resource.pure(Nil)

    val geminiR: Resource[IO, List[ProviderAdapter[IO]]] =
      geminiKey match
        case Some(key) =>
          GeminiAdapter.resource(key, geminiBaseUrl).map(a => List(a))
        case None => Resource.pure(Nil)

    for
      a <- anthropicR
      o <- openaiR
      g <- geminiR
    yield a ++ o ++ g
