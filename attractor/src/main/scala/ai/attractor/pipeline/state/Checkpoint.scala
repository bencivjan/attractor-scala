package ai.attractor.pipeline.state

import cats.effect.IO
import fs2.io.file.{Files, Path}
import io.circe.*
import io.circe.syntax.*
import io.circe.parser.decode

// ---------------------------------------------------------------------------
// Serializable checkpoint for pipeline resume/replay.
//
// A checkpoint captures enough state to resume a pipeline from where it
// left off, including the current node, completed nodes, retry counts,
// context values, and execution logs.
// ---------------------------------------------------------------------------

case class Checkpoint(
    timestamp: java.time.Instant,
    currentNode: String,
    completedNodes: List[String],
    nodeRetries: Map[String, Int],
    contextValues: Map[String, Any],
    logs: List[String]
):
  /** Serialize this checkpoint to a JSON file at the given path. */
  def save(path: String): IO[Unit] =
    val json = Checkpoint.encode(this).noSpaces
    val bytes = fs2.Stream.emits(json.getBytes("UTF-8"))
    bytes.through(Files[IO].writeAll(Path(path))).compile.drain

object Checkpoint:

  /** Deserialize a checkpoint from a JSON file at the given path. */
  def load(path: String): IO[Checkpoint] =
    Files[IO]
      .readAll(Path(path))
      .through(fs2.text.utf8.decode)
      .compile
      .string
      .flatMap: content =>
        decode[Checkpoint](content) match
          case Right(cp) => IO.pure(cp)
          case Left(err) => IO.raiseError(new RuntimeException(s"Failed to decode checkpoint: $err"))

  // -------------------------------------------------------------------------
  // JSON codecs
  // -------------------------------------------------------------------------

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

  def encode(cp: Checkpoint): Json =
    Json.obj(
      "timestamp" -> Json.fromString(cp.timestamp.toString),
      "current_node" -> Json.fromString(cp.currentNode),
      "completed_nodes" -> cp.completedNodes.asJson,
      "node_retries" -> cp.nodeRetries.asJson,
      "context_values" -> Json.obj(
        cp.contextValues.map((k, v) => k -> anyToJson(v)).toSeq*
      ),
      "logs" -> cp.logs.asJson
    )

  given Encoder[Checkpoint] = Encoder.instance(encode)

  given Decoder[Checkpoint] = Decoder.instance: c =>
    for
      timestamp      <- c.downField("timestamp").as[String]
      currentNode    <- c.downField("current_node").as[String]
      completedNodes <- c.downField("completed_nodes").as[List[String]]
      nodeRetries    <- c.downField("node_retries").as[Map[String, Int]]
      contextJson    <- c.downField("context_values").as[JsonObject]
      logs           <- c.downField("logs").as[List[String]]
    yield
      val contextValues = contextJson.toMap.view.mapValues(jsonToAny).toMap
      Checkpoint(
        timestamp = java.time.Instant.parse(timestamp),
        currentNode = currentNode,
        completedNodes = completedNodes,
        nodeRetries = nodeRetries,
        contextValues = contextValues,
        logs = logs
      )
