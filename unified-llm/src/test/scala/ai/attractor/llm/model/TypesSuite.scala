package ai.attractor.llm.model

import munit.FunSuite
import io.circe.syntax.*
import io.circe.parser.decode

class TypesSuite extends FunSuite:

  // ---------------------------------------------------------------------------
  // Role
  // ---------------------------------------------------------------------------

  test("Role round-trips through JSON"):
    val roles = List(Role.System, Role.User, Role.Assistant, Role.Tool, Role.Developer)
    roles.foreach: role =>
      val json = role.asJson
      val decoded = json.as[Role]
      assertEquals(decoded, Right(role))

  // ---------------------------------------------------------------------------
  // ContentPart
  // ---------------------------------------------------------------------------

  test("ContentPart.text factory sets kind and text"):
    val cp = ContentPart.text("hello")
    assertEquals(cp.kind, ContentKind.Text)
    assertEquals(cp.text, Some("hello"))

  test("ContentPart.toolCall factory sets kind and data"):
    val data = ToolCallData("id1", "myTool", io.circe.Json.obj())
    val cp = ContentPart.toolCall(data)
    assertEquals(cp.kind, ContentKind.ToolCall)
    assertEquals(cp.toolCall, Some(data))

  test("ContentPart JSON round-trip"):
    val cp = ContentPart.text("test content")
    val json = cp.asJson
    val decoded = json.as[ContentPart]
    assertEquals(decoded.map(_.text), Right(Some("test content")))
    assertEquals(decoded.map(_.kind), Right(ContentKind.Text))

  // ---------------------------------------------------------------------------
  // Message
  // ---------------------------------------------------------------------------

  test("Message.user creates user message"):
    val msg = Message.user("hello world")
    assertEquals(msg.role, Role.User)
    assertEquals(msg.text, "hello world")

  test("Message.system creates system message"):
    val msg = Message.system("be helpful")
    assertEquals(msg.role, Role.System)
    assertEquals(msg.text, "be helpful")

  test("Message JSON round-trip"):
    val msg = Message.assistant("response text")
    val json = msg.asJson
    val decoded = json.as[Message]
    assert(decoded.isRight)
    assertEquals(decoded.map(_.role), Right(Role.Assistant))
    assertEquals(decoded.map(_.text), Right("response text"))

  // ---------------------------------------------------------------------------
  // Usage
  // ---------------------------------------------------------------------------

  test("Usage.empty has zero tokens"):
    assertEquals(Usage.empty.inputTokens, 0)
    assertEquals(Usage.empty.outputTokens, 0)
    assertEquals(Usage.empty.totalTokens, 0)

  test("Usage addition sums token counts"):
    val a = Usage(inputTokens = 100, outputTokens = 50, totalTokens = 150)
    val b = Usage(inputTokens = 200, outputTokens = 100, totalTokens = 300)
    val sum = a + b
    assertEquals(sum.inputTokens, 300)
    assertEquals(sum.outputTokens, 150)
    assertEquals(sum.totalTokens, 450)

  test("Usage addition merges optional fields"):
    val a = Usage(reasoningTokens = Some(10))
    val b = Usage(reasoningTokens = Some(20))
    val sum = a + b
    assertEquals(sum.reasoningTokens, Some(30))

  test("Usage addition handles one-sided optional fields"):
    val a = Usage(cacheReadTokens = Some(100))
    val b = Usage()
    val sum = a + b
    assertEquals(sum.cacheReadTokens, Some(100))

  test("Usage JSON round-trip"):
    val u = Usage(inputTokens = 50, outputTokens = 25, totalTokens = 75, reasoningTokens = Some(10))
    val json = u.asJson
    val decoded = json.as[Usage]
    assert(decoded.isRight)
    assertEquals(decoded.map(_.inputTokens), Right(50))
    assertEquals(decoded.map(_.reasoningTokens), Right(Some(10)))

  // ---------------------------------------------------------------------------
  // FinishReason
  // ---------------------------------------------------------------------------

  test("FinishReason constants"):
    assertEquals(FinishReason.stop.reason, "stop")
    assertEquals(FinishReason.length.reason, "length")
    assertEquals(FinishReason.toolCalls.reason, "tool_calls")

  // ---------------------------------------------------------------------------
  // ToolChoice
  // ---------------------------------------------------------------------------

  test("ToolChoice.auto mode"):
    assertEquals(ToolChoice.auto.mode, "auto")
    assertEquals(ToolChoice.auto.toolName, None)

  test("ToolChoice.named carries tool name"):
    val tc = ToolChoice.named("myTool")
    assertEquals(tc.mode, "named")
    assertEquals(tc.toolName, Some("myTool"))

  // ---------------------------------------------------------------------------
  // ResponseFormat
  // ---------------------------------------------------------------------------

  test("ResponseFormat JSON round-trip"):
    val rf = ResponseFormat("json_schema", jsonSchema = Some(io.circe.Json.obj()), strict = true)
    val json = rf.asJson
    val decoded = json.as[ResponseFormat]
    assert(decoded.isRight)
    assertEquals(decoded.map(_.formatType), Right("json_schema"))
    assertEquals(decoded.map(_.strict), Right(true))

  // ---------------------------------------------------------------------------
  // Request
  // ---------------------------------------------------------------------------

  test("Request JSON round-trip"):
    val req = Request(
      model = "claude-sonnet-4-20250514",
      messages = List(Message.user("hello")),
      tools = List(ToolDefinition("myTool", "A tool", io.circe.Json.obj())),
      reasoningEffort = Some("high")
    )
    val json = req.asJson
    val decoded = json.as[Request]
    assert(decoded.isRight)
    assertEquals(decoded.map(_.model), Right("claude-sonnet-4-20250514"))
    assertEquals(decoded.map(_.reasoningEffort), Right(Some("high")))
