package ai.attractor.pipeline.handler

import cats.effect.IO

import ai.attractor.llm.model.{Message, ModelCatalog, Request}
import ai.attractor.llm.provider.Client
import ai.attractor.pipeline.parser.Node
import ai.attractor.pipeline.state.{Context, Outcome}

/** CodergenBackend that routes LLM calls through the unified-llm Client.
  *
  * Uses the node's `model` and `llm_provider` attributes to build a Request.
  * If `llm_provider` is not set, falls back to ModelCatalog lookup to resolve
  * the provider from the model name.
  */
class UnifiedLlmBackend(
    client: Client,
    defaultModel: String = "claude-sonnet-4-5"
) extends CodergenBackend:

  def run(node: Node, prompt: String, context: Context): IO[Either[Outcome, String]] =
    val model = node.model.getOrElse(defaultModel)
    val provider = node.llmProvider.orElse(
      ModelCatalog.getModelInfo(model).map(_.provider)
    )
    val request = Request(
      model = model,
      messages = List(Message.user(prompt)),
      provider = provider,
      maxTokens = Some(16_000)
    )
    client
      .complete(request)
      .map(response => Right(response.text))
      .handleError: err =>
        Left(Outcome.fail(
          failureReason = s"LLM call failed: ${err.getMessage}",
          notes = s"Model=$model, provider=${provider.getOrElse("auto")}"
        ))
