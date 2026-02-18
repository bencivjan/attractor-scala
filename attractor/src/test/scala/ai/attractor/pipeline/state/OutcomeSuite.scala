package ai.attractor.pipeline.state

import munit.FunSuite
import io.circe.syntax.*
import io.circe.parser.decode

class OutcomeSuite extends FunSuite:

  test("Outcome.success creates success outcome"):
    val o = Outcome.success(notes = "all good")
    assertEquals(o.status, StageStatus.Success)
    assertEquals(o.notes, "all good")

  test("Outcome.fail creates fail outcome"):
    val o = Outcome.fail(failureReason = "broken")
    assertEquals(o.status, StageStatus.Fail)
    assertEquals(o.failureReason, "broken")

  test("Outcome.retry creates retry outcome"):
    val o = Outcome.retry(notes = "try again")
    assertEquals(o.status, StageStatus.Retry)

  test("Outcome JSON round-trip"):
    val o = Outcome(
      status = StageStatus.Success,
      preferredLabel = "approve",
      suggestedNextIds = List("next1", "next2"),
      contextUpdates = Map("key" -> "value", "count" -> 42),
      notes = "completed"
    )
    val json = o.asJson
    val decoded = json.as[Outcome]
    assert(decoded.isRight, s"Decode failed: $decoded")
    val d = decoded.toOption.get
    assertEquals(d.status, StageStatus.Success)
    assertEquals(d.preferredLabel, "approve")
    assertEquals(d.suggestedNextIds, List("next1", "next2"))
    assertEquals(d.notes, "completed")

  test("StageStatus round-trips through JSON"):
    val statuses = List(
      StageStatus.Success, StageStatus.PartialSuccess,
      StageStatus.Retry, StageStatus.Fail, StageStatus.Skipped
    )
    statuses.foreach: status =>
      val json = status.asJson
      val decoded = json.as[StageStatus]
      assertEquals(decoded, Right(status))

  test("Outcome with empty context updates round-trips"):
    val o = Outcome.success()
    val json = o.asJson
    val decoded = json.as[Outcome]
    assert(decoded.isRight)
    assertEquals(decoded.toOption.get.contextUpdates, Map.empty)
