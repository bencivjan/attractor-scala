package ai.attractor.llm.model

import munit.FunSuite

class ModelCatalogSuite extends FunSuite:

  test("getModelInfo returns info for known model"):
    val info = ModelCatalog.getModelInfo("claude-sonnet-4-5-20250514")
    assert(info.isDefined)
    assertEquals(info.get.provider, "anthropic")

  test("getModelInfo returns None for unknown model"):
    val info = ModelCatalog.getModelInfo("unknown-model-xyz")
    assertEquals(info, None)

  test("listModels returns non-empty list"):
    val models = ModelCatalog.listModels()
    assert(models.nonEmpty)

  test("listModels can filter by provider"):
    val anthropicModels = ModelCatalog.listModels(provider = Some("anthropic"))
    assert(anthropicModels.nonEmpty)
    assert(anthropicModels.forall(_.provider == "anthropic"))

  test("listModels can filter by OpenAI"):
    val openaiModels = ModelCatalog.listModels(provider = Some("openai"))
    assert(openaiModels.nonEmpty)
    assert(openaiModels.forall(_.provider == "openai"))

  test("listModels can filter by Gemini"):
    val geminiModels = ModelCatalog.listModels(provider = Some("google"))
    assert(geminiModels.nonEmpty)
    assert(geminiModels.forall(_.provider == "google"))

  test("model info has valid context window"):
    val models = ModelCatalog.listModels()
    models.foreach: m =>
      assert(m.contextWindow > 0, s"Model ${m.id} has invalid context window: ${m.contextWindow}")
