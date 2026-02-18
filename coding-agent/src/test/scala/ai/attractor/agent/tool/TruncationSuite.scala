package ai.attractor.agent.tool

import munit.FunSuite
import ai.attractor.agent.session.SessionConfig

class TruncationSuite extends FunSuite:

  test("short output is not truncated"):
    val config = SessionConfig()
    val output = "hello world"
    val result = Truncation.truncateToolOutput(output, "read_file", config)
    assertEquals(result, output)

  test("character truncation runs on long output"):
    val config = SessionConfig()
    // read_file default limit is 50000 chars
    val output = "x" * 60000
    val result = Truncation.truncateToolOutput(output, "read_file", config)
    assert(result.length < 60000)
    assert(result.contains("[WARNING: Tool output was truncated."))

  test("truncation inserts visible warning marker"):
    val config = SessionConfig()
    val output = "x" * 60000
    val result = Truncation.truncateToolOutput(output, "read_file", config)
    assert(result.contains("characters removed."))

  test("character truncation applies FIRST before line truncation"):
    val config = SessionConfig()
    // Create output with many short lines (within char limit) but exceeding line limit
    // shell default: 30000 chars, 256 lines
    val lines = (1 to 300).map(i => s"line $i").mkString("\n")
    val result = Truncation.truncateToolOutput(lines, "shell", config)
    // Should have at most ~256 lines (line truncation applies second)
    val resultLines = result.split("\n").length
    assert(resultLines <= 260, s"Expected <= 260 lines, got $resultLines") // allow margin for warning

  test("unknown tool name uses default limits"):
    val config = SessionConfig()
    val output = "x" * 100
    val result = Truncation.truncateToolOutput(output, "unknown_tool", config)
    assertEquals(result, output)
