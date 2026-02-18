package ai.attractor.pipeline.parser

// ---------------------------------------------------------------------------
// In-memory graph model for parsed DOT pipeline definitions
// ---------------------------------------------------------------------------

case class Graph(
    name: String,
    nodes: Map[String, Node],
    edges: List[Edge],
    attributes: Map[String, String] = Map.empty
):
  def goal: String = attributes.getOrElse("goal", "")
  def label: String = attributes.getOrElse("label", "")
  def modelStylesheet: String = attributes.getOrElse("model_stylesheet", "")
  def defaultMaxRetry: Int =
    attributes.get("default_max_retry").flatMap(_.toIntOption).getOrElse(50)
  def retryTarget: Option[String] =
    attributes.get("retry_target").filter(_.nonEmpty)
  def fallbackRetryTarget: Option[String] =
    attributes.get("fallback_retry_target").filter(_.nonEmpty)
  def defaultFidelity: String = attributes.getOrElse("default_fidelity", "")

  def outgoingEdges(nodeId: String): List[Edge] =
    edges.filter(_.from == nodeId)

  def incomingEdges(nodeId: String): List[Edge] =
    edges.filter(_.to == nodeId)

case class Node(
    id: String,
    attributes: Map[String, String] = Map.empty
):
  def label: String = attributes.getOrElse("label", id)
  def shape: String = attributes.getOrElse("shape", "box")
  def nodeType: String = attributes.getOrElse("type", "")
  def prompt: String = attributes.getOrElse("prompt", "")
  def maxRetries: Int =
    attributes.get("max_retries").flatMap(_.toIntOption).getOrElse(0)
  def goalGate: Boolean = attributes.get("goal_gate").contains("true")
  def retryTarget: Option[String] =
    attributes.get("retry_target").filter(_.nonEmpty)
  def fallbackRetryTarget: Option[String] =
    attributes.get("fallback_retry_target").filter(_.nonEmpty)
  def fidelity: Option[String] =
    attributes.get("fidelity").filter(_.nonEmpty)
  def threadId: Option[String] =
    attributes.get("thread_id").filter(_.nonEmpty)
  def classes: List[String] =
    attributes
      .getOrElse("class", "")
      .split(",")
      .map(_.trim)
      .filter(_.nonEmpty)
      .toList
  def timeout: Option[String] =
    attributes.get("timeout").filter(_.nonEmpty)
  def model: Option[String] =
    attributes.get("model").orElse(attributes.get("llm_model")).filter(_.nonEmpty)
  def llmModel: Option[String] = model
  def llmProvider: Option[String] =
    attributes.get("llm_provider").filter(_.nonEmpty)
  def reasoningEffort: String =
    attributes.getOrElse("reasoning_effort", "high")
  def autoStatus: Boolean =
    attributes.get("auto_status").contains("true")
  def allowPartial: Boolean =
    attributes.get("allow_partial").contains("true")

case class Edge(
    from: String,
    to: String,
    attributes: Map[String, String] = Map.empty
):
  def label: String = attributes.getOrElse("label", "")
  def condition: String = attributes.getOrElse("condition", "")
  def weight: Int =
    attributes.get("weight").flatMap(_.toIntOption).getOrElse(1)
  def fidelity: Option[String] =
    attributes.get("fidelity").filter(_.nonEmpty)
  def threadId: Option[String] =
    attributes.get("thread_id").filter(_.nonEmpty)
  def loopRestart: Boolean =
    attributes.get("loop_restart").contains("true")
