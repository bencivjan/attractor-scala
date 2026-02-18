package ai.attractor.llm.api

import ai.attractor.llm.model.*
import ai.attractor.llm.provider.Client
import ai.attractor.llm.util.RetryPolicy
import cats.effect.IO
import io.circe.{Json, JsonObject}
import io.circe.syntax.*

/** Structured output generation that produces a validated JSON object
  * conforming to a caller-supplied JSON Schema.
  *
  * The implementation strategy depends on the provider:
  *   - **OpenAI / Google**: use the native `json_schema` response format so the
  *     model's decoding is constrained to valid JSON matching the schema.
  *   - **Anthropic**: use a tool-based extraction pattern -- define a synthetic
  *     tool whose input schema matches the desired output, force the model to
  *     call it, and extract the arguments as the structured output.
  */
object GenerateObject:

  /** Name of the synthetic tool used for Anthropic-style extraction. */
  private val ExtractionToolName = "__extract_structured_output"

  def apply(
      model: String,
      prompt: String,
      schema: Json,
      system: Option[String] = None,
      provider: Option[String] = None,
      providerOptions: Option[Json] = None,
      maxRetries: Int = 2,
      client: Option[Client] = None
  ): IO[GenerateResult] =
    val resolvedProvider = provider.orElse(inferProvider(model))
    val policy = RetryPolicy(maxRetries = maxRetries)

    RetryPolicy.retry(policy):
      resolvedProvider match
        case Some(p) if p.toLowerCase == "anthropic" =>
          generateViaToolExtraction(model, prompt, schema, system, provider, providerOptions, client)
        case _ =>
          generateViaNativeFormat(model, prompt, schema, system, provider, providerOptions, client)

  // ---------------------------------------------------------------------------
  // Strategy 1: Native json_schema response format (OpenAI, Gemini)
  // ---------------------------------------------------------------------------

  private def generateViaNativeFormat(
      model: String,
      prompt: String,
      schema: Json,
      system: Option[String],
      provider: Option[String],
      providerOptions: Option[Json],
      client: Option[Client]
  ): IO[GenerateResult] =
    val responseFormat = ResponseFormat(
      formatType = "json_schema",
      jsonSchema = Some(schema),
      strict = true
    )

    for
      result <- Generate(
        model = model,
        prompt = Some(prompt),
        system = system,
        responseFormat = Some(responseFormat),
        provider = provider,
        providerOptions = providerOptions,
        maxRetries = 0, // retries handled at this level
        client = client
      )
      parsed <- parseOutput(result.text, model, provider)
    yield result.copy(output = Some(parsed))

  // ---------------------------------------------------------------------------
  // Strategy 2: Tool-based extraction (Anthropic)
  // ---------------------------------------------------------------------------

  private def generateViaToolExtraction(
      model: String,
      prompt: String,
      schema: Json,
      system: Option[String],
      provider: Option[String],
      providerOptions: Option[Json],
      client: Option[Client]
  ): IO[GenerateResult] =
    val extractionTool = Tool(
      name = ExtractionToolName,
      description = "Extract structured data from the conversation according to the provided schema.",
      parameters = schema
    )

    for
      result <- Generate(
        model = model,
        prompt = Some(prompt),
        system = system,
        tools = Some(List(extractionTool)),
        toolChoice = Some(ToolChoice.named(ExtractionToolName)),
        maxToolRounds = 0, // do not actually execute the synthetic tool
        provider = provider,
        providerOptions = providerOptions,
        maxRetries = 0,
        client = client
      )
      parsed <- extractToolArguments(result, model, provider)
    yield result.copy(output = Some(parsed))

  // ---------------------------------------------------------------------------
  // Output parsing and validation
  // ---------------------------------------------------------------------------

  private def parseOutput(
      text: String,
      model: String,
      provider: Option[String]
  ): IO[Json] =
    io.circe.parser.parse(text) match
      case Right(json) => IO.pure(json)
      case Left(err) =>
        IO.raiseError(
          new NoObjectGeneratedError(
            s"Failed to parse model output as JSON: ${err.getMessage}",
            provider = provider.getOrElse(inferProvider(model).getOrElse("unknown")),
            raw = Some(Json.fromString(text))
          )
        )

  private def extractToolArguments(
      result: GenerateResult,
      model: String,
      provider: Option[String]
  ): IO[Json] =
    result.toolCalls
      .find(_.name == ExtractionToolName)
      .map(tc => IO.pure(tc.arguments))
      .getOrElse:
        IO.raiseError(
          new NoObjectGeneratedError(
            s"Model did not produce the expected extraction tool call",
            provider = provider.getOrElse(inferProvider(model).getOrElse("unknown")),
            raw = Some(Json.fromString(result.text))
          )
        )

  // ---------------------------------------------------------------------------
  // Provider inference from model name
  // ---------------------------------------------------------------------------

  private def inferProvider(model: String): Option[String] =
    ModelCatalog.getModelInfo(model).map(_.provider).orElse:
      val lower = model.toLowerCase
      if lower.startsWith("claude") then Some("anthropic")
      else if lower.startsWith("gpt") || lower.startsWith("o1") || lower.startsWith("o3") then Some("openai")
      else if lower.startsWith("gemini") then Some("google")
      else None
