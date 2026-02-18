package ai.attractor.llm.api

import ai.attractor.llm.model.*
import ai.attractor.llm.provider.Client
import cats.effect.{IO, Ref}
import io.circe.Json

/** Module-level singleton that provides a lazily initialized default
  * [[Client]] instance.
  *
  * On first access via [[get]], the client is constructed from environment
  * variables (e.g. `LLM_PROVIDER`, API keys). Callers may also explicitly
  * [[set]] a client -- useful in tests or when multiple providers are in
  * play.
  *
  * Convenience functions ([[generate]], [[stream]], [[generateObject]])
  * delegate to the corresponding high-level APIs using this default client.
  */
object DefaultClient:

  /** The lazily-populated singleton reference. Initialization is deferred
    * to the first [[get]] call so that the IORuntime does not need to be
    * available at class-loading time.
    */
  private val instance: Ref[IO, Option[Client]] =
    // Safe because Ref.unsafe merely allocates an AtomicReference; no IO
    // effects run until `get` or `set` is called.
    Ref.unsafe[IO, Option[Client]](None)

  // -------------------------------------------------------------------------
  // Core accessors
  // -------------------------------------------------------------------------

  /** Return the current default client, initializing from the environment
    * if none has been set yet.
    */
  def get: IO[Client] =
    instance.get.flatMap:
      case Some(c) => IO.pure(c)
      case None =>
        initFromEnv.flatMap: c =>
          instance.set(Some(c)).as(c)

  /** Explicitly set the default client. Subsequent calls to [[get]] and to
    * the convenience functions will use this client.
    */
  def set(client: Client): IO[Unit] =
    instance.set(Some(client))

  // -------------------------------------------------------------------------
  // Environment-based initialization
  // -------------------------------------------------------------------------

  /** Build a client from environment variables.
    *
    * Because `Client.fromEnv()` returns a `Resource`, we allocate it here
    * and store the finalizer so that it can be released via [[close]].
    * For a module-level singleton whose lifetime matches the process this
    * is the expected pattern.
    */
  private def initFromEnv: IO[Client] =
    Client.fromEnv().allocated.flatMap: (client, release) =>
      finalizer.set(Some(release)).as(client)

  /** Optional resource finalizer stored when the client is lazily created
    * from environment variables.  Calling [[close]] runs it.
    */
  private val finalizer: Ref[IO, Option[IO[Unit]]] =
    Ref.unsafe[IO, Option[IO[Unit]]](None)

  /** Release the underlying HTTP resources if the default client was
    * initialized from the environment.  No-op if [[set]] was used.
    */
  def close: IO[Unit] =
    finalizer.getAndSet(None).flatMap(_.getOrElse(IO.unit))

  // -------------------------------------------------------------------------
  // Convenience top-level functions
  // -------------------------------------------------------------------------

  def generate(
      model: String,
      prompt: Option[String] = None,
      messages: Option[List[Message]] = None,
      system: Option[String] = None,
      tools: Option[List[Tool]] = None,
      toolChoice: Option[ToolChoice] = None,
      maxToolRounds: Int = 1,
      stopWhen: Option[List[StepResult] => Boolean] = None,
      responseFormat: Option[ResponseFormat] = None,
      temperature: Option[Double] = None,
      topP: Option[Double] = None,
      maxTokens: Option[Int] = None,
      stopSequences: Option[List[String]] = None,
      reasoningEffort: Option[String] = None,
      provider: Option[String] = None,
      providerOptions: Option[Json] = None,
      maxRetries: Int = 2
  ): IO[GenerateResult] =
    Generate(
      model = model,
      prompt = prompt,
      messages = messages,
      system = system,
      tools = tools,
      toolChoice = toolChoice,
      maxToolRounds = maxToolRounds,
      stopWhen = stopWhen,
      responseFormat = responseFormat,
      temperature = temperature,
      topP = topP,
      maxTokens = maxTokens,
      stopSequences = stopSequences,
      reasoningEffort = reasoningEffort,
      provider = provider,
      providerOptions = providerOptions,
      maxRetries = maxRetries,
      client = None // uses DefaultClient.get internally
    )

  def stream(
      model: String,
      prompt: Option[String] = None,
      messages: Option[List[Message]] = None,
      system: Option[String] = None,
      tools: Option[List[Tool]] = None,
      toolChoice: Option[ToolChoice] = None,
      maxToolRounds: Int = 1,
      responseFormat: Option[ResponseFormat] = None,
      temperature: Option[Double] = None,
      topP: Option[Double] = None,
      maxTokens: Option[Int] = None,
      stopSequences: Option[List[String]] = None,
      reasoningEffort: Option[String] = None,
      provider: Option[String] = None,
      providerOptions: Option[Json] = None
  ): IO[StreamResult] =
    StreamApi(
      model = model,
      prompt = prompt,
      messages = messages,
      system = system,
      tools = tools,
      toolChoice = toolChoice,
      maxToolRounds = maxToolRounds,
      responseFormat = responseFormat,
      temperature = temperature,
      topP = topP,
      maxTokens = maxTokens,
      stopSequences = stopSequences,
      reasoningEffort = reasoningEffort,
      provider = provider,
      providerOptions = providerOptions,
      client = None
    )

  def generateObject(
      model: String,
      prompt: String,
      schema: Json,
      system: Option[String] = None,
      provider: Option[String] = None,
      providerOptions: Option[Json] = None,
      maxRetries: Int = 2
  ): IO[GenerateResult] =
    GenerateObject(
      model = model,
      prompt = prompt,
      schema = schema,
      system = system,
      provider = provider,
      providerOptions = providerOptions,
      maxRetries = maxRetries,
      client = None
    )
