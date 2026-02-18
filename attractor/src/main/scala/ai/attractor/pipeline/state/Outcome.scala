package ai.attractor.pipeline.state

import io.circe.*
import io.circe.syntax.*

// ---------------------------------------------------------------------------
// Stage execution status
// ---------------------------------------------------------------------------

enum StageStatus:
  case Success, PartialSuccess, Retry, Fail, Skipped

object StageStatus:
  given Encoder[StageStatus] = Encoder[String].contramap(_.toString.toLowerCase)
  given Decoder[StageStatus] = Decoder[String].emap: s =>
    s.toLowerCase match
      case "success"         => Right(StageStatus.Success)
      case "partialsuccess"  => Right(StageStatus.PartialSuccess)
      case "partial_success" => Right(StageStatus.PartialSuccess)
      case "retry"           => Right(StageStatus.Retry)
      case "fail"            => Right(StageStatus.Fail)
      case "skipped"         => Right(StageStatus.Skipped)
      case other             => Left(s"Unknown stage status: $other")

// ---------------------------------------------------------------------------
// Outcome of executing a single pipeline stage
// ---------------------------------------------------------------------------

case class Outcome(
    status: StageStatus,
    preferredLabel: String = "",
    suggestedNextIds: List[String] = Nil,
    contextUpdates: Map[String, Any] = Map.empty,
    notes: String = "",
    failureReason: String = ""
)

object Outcome:
  def success(
      preferredLabel: String = "",
      suggestedNextIds: List[String] = Nil,
      contextUpdates: Map[String, Any] = Map.empty,
      notes: String = ""
  ): Outcome =
    Outcome(StageStatus.Success, preferredLabel, suggestedNextIds, contextUpdates, notes)

  def fail(
      failureReason: String = "",
      contextUpdates: Map[String, Any] = Map.empty,
      notes: String = ""
  ): Outcome =
    Outcome(StageStatus.Fail, contextUpdates = contextUpdates, notes = notes, failureReason = failureReason)

  def retry(
      preferredLabel: String = "",
      notes: String = ""
  ): Outcome =
    Outcome(StageStatus.Retry, preferredLabel = preferredLabel, notes = notes)

  /** Encode Any values best-effort for JSON serialization. */
  private def anyToJson(value: Any): Json = value match
    case null       => Json.Null
    case s: String  => Json.fromString(s)
    case i: Int     => Json.fromInt(i)
    case l: Long    => Json.fromLong(l)
    case d: Double  => Json.fromDoubleOrNull(d)
    case b: Boolean => Json.fromBoolean(b)
    case m: Map[?, ?] =>
      Json.obj(m.map((k, v) => k.toString -> anyToJson(v)).toSeq*)
    case l: List[?] =>
      Json.arr(l.map(anyToJson)*)
    case other => Json.fromString(other.toString)

  /** Decode a JSON value back to Any (strings, numbers, booleans, maps, lists). */
  private def jsonToAny(json: Json): Any =
    json.fold(
      jsonNull = null,
      jsonBoolean = identity,
      jsonNumber = n => n.toInt.getOrElse(n.toLong.getOrElse(n.toDouble)),
      jsonString = identity,
      jsonArray = arr => arr.map(jsonToAny).toList,
      jsonObject = obj => obj.toMap.view.mapValues(jsonToAny).toMap
    )

  given Encoder[Outcome] = Encoder.instance: o =>
    val updatesJson = Json.obj(
      o.contextUpdates.map((k, v) => k -> anyToJson(v)).toSeq*
    )
    Json.obj(
      "status" -> o.status.asJson,
      "preferred_label" -> o.preferredLabel.asJson,
      "suggested_next_ids" -> o.suggestedNextIds.asJson,
      "context_updates" -> updatesJson,
      "notes" -> o.notes.asJson,
      "failure_reason" -> o.failureReason.asJson
    ).dropNullValues

  given Decoder[Outcome] = Decoder.instance: c =>
    for
      status           <- c.downField("status").as[StageStatus]
      preferredLabel   <- c.downField("preferred_label").as[Option[String]]
      suggestedNextIds <- c.downField("suggested_next_ids").as[Option[List[String]]]
      contextJson      <- c.downField("context_updates").as[Option[JsonObject]]
      notes            <- c.downField("notes").as[Option[String]]
      failureReason    <- c.downField("failure_reason").as[Option[String]]
    yield
      val contextUpdates = contextJson
        .map(_.toMap.view.mapValues(jsonToAny).toMap)
        .getOrElse(Map.empty)
      Outcome(
        status = status,
        preferredLabel = preferredLabel.getOrElse(""),
        suggestedNextIds = suggestedNextIds.getOrElse(Nil),
        contextUpdates = contextUpdates,
        notes = notes.getOrElse(""),
        failureReason = failureReason.getOrElse("")
      )
