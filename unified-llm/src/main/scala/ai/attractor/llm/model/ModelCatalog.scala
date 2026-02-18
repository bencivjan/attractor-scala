package ai.attractor.llm.model

object ModelCatalog:

  // -------------------------------------------------------------------------
  // Anthropic models
  // -------------------------------------------------------------------------

  private val claudeOpus4_6: ModelInfo = ModelInfo(
    id = "claude-opus-4-6",
    provider = "anthropic",
    displayName = "Claude Opus 4.6",
    contextWindow = 200_000,
    maxOutput = 32_000,
    supportsTools = true,
    supportsVision = true,
    supportsReasoning = true,
    inputCostPerMillion = BigDecimal("15.00"),
    outputCostPerMillion = BigDecimal("75.00"),
    aliases = List("claude-opus-4-6-20250610")
  )

  private val claudeSonnet4_5: ModelInfo = ModelInfo(
    id = "claude-sonnet-4-5",
    provider = "anthropic",
    displayName = "Claude Sonnet 4.5",
    contextWindow = 200_000,
    maxOutput = 16_000,
    supportsTools = true,
    supportsVision = true,
    supportsReasoning = true,
    inputCostPerMillion = BigDecimal("3.00"),
    outputCostPerMillion = BigDecimal("15.00"),
    aliases = List("claude-sonnet-4-5-20250514")
  )

  // -------------------------------------------------------------------------
  // OpenAI models
  // -------------------------------------------------------------------------

  private val gpt5_2: ModelInfo = ModelInfo(
    id = "gpt-5.2",
    provider = "openai",
    displayName = "GPT-5.2",
    contextWindow = 128_000,
    maxOutput = 16_384,
    supportsTools = true,
    supportsVision = true,
    supportsReasoning = true,
    inputCostPerMillion = BigDecimal("5.00"),
    outputCostPerMillion = BigDecimal("15.00"),
    aliases = List("gpt-5.2-2025-02-01")
  )

  private val gpt5_2mini: ModelInfo = ModelInfo(
    id = "gpt-5.2-mini",
    provider = "openai",
    displayName = "GPT-5.2 Mini",
    contextWindow = 128_000,
    maxOutput = 16_384,
    supportsTools = true,
    supportsVision = true,
    supportsReasoning = true,
    inputCostPerMillion = BigDecimal("0.30"),
    outputCostPerMillion = BigDecimal("1.20"),
    aliases = List("gpt-5.2-mini-2025-02-01")
  )

  private val gpt5_2codex: ModelInfo = ModelInfo(
    id = "gpt-5.2-codex",
    provider = "openai",
    displayName = "GPT-5.2 Codex",
    contextWindow = 128_000,
    maxOutput = 16_384,
    supportsTools = true,
    supportsVision = false,
    supportsReasoning = true,
    inputCostPerMillion = BigDecimal("3.00"),
    outputCostPerMillion = BigDecimal("12.00"),
    aliases = List("gpt-5.2-codex-2025-02-01")
  )

  // -------------------------------------------------------------------------
  // Google Gemini models
  // -------------------------------------------------------------------------

  private val gemini3ProPreview: ModelInfo = ModelInfo(
    id = "gemini-3-pro-preview",
    provider = "google",
    displayName = "Gemini 3 Pro Preview",
    contextWindow = 1_000_000,
    maxOutput = 8_192,
    supportsTools = true,
    supportsVision = true,
    supportsReasoning = true,
    inputCostPerMillion = BigDecimal("3.50"),
    outputCostPerMillion = BigDecimal("10.50"),
    aliases = List("gemini-3.0-pro-preview")
  )

  private val gemini3FlashPreview: ModelInfo = ModelInfo(
    id = "gemini-3-flash-preview",
    provider = "google",
    displayName = "Gemini 3 Flash Preview",
    contextWindow = 1_000_000,
    maxOutput = 8_192,
    supportsTools = true,
    supportsVision = true,
    supportsReasoning = false,
    inputCostPerMillion = BigDecimal("0.15"),
    outputCostPerMillion = BigDecimal("0.60"),
    aliases = List("gemini-3.0-flash-preview")
  )

  // -------------------------------------------------------------------------
  // Catalog registry
  // -------------------------------------------------------------------------

  private val allModels: List[ModelInfo] = List(
    claudeOpus4_6,
    claudeSonnet4_5,
    gpt5_2,
    gpt5_2mini,
    gpt5_2codex,
    gemini3ProPreview,
    gemini3FlashPreview
  )

  /** Index by primary id and all aliases. */
  private val byIdOrAlias: Map[String, ModelInfo] =
    allModels.flatMap(m => (m.id :: m.aliases).map(_ -> m)).toMap

  // -------------------------------------------------------------------------
  // Public API
  // -------------------------------------------------------------------------

  /** Look up a model by its id or any of its aliases. */
  def getModelInfo(idOrAlias: String): Option[ModelInfo] =
    byIdOrAlias.get(idOrAlias)

  /** List all models, optionally filtered by provider. */
  def listModels(provider: Option[String] = None): List[ModelInfo] =
    provider match
      case Some(p) => allModels.filter(_.provider == p)
      case None    => allModels

  /** Get the latest (first listed) model for a given provider. */
  def getLatestModel(provider: String): Option[ModelInfo] =
    allModels.find(_.provider == provider)
