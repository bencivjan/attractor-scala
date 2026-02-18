package ai.attractor.pipeline.stylesheet

import munit.FunSuite

class StylesheetParserSuite extends FunSuite:

  test("parse simple stylesheet with colon syntax"):
    val css = """
      |* {
      |  model: claude-opus-4-6;
      |  reasoning_effort: high;
      |}
    """.stripMargin
    val rules = StylesheetParser.parse(css)
    assert(rules.isRight, s"Parse failed: $rules")
    val ruleList = rules.toOption.get
    assert(ruleList.nonEmpty)

  test("parse shape selector"):
    val css = """
      |box {
      |  model: claude-opus-4-6;
      |}
    """.stripMargin
    val rules = StylesheetParser.parse(css)
    assert(rules.isRight)
    val ruleList = rules.toOption.get
    assertEquals(ruleList.head.selector, Selector.Shape("box"))

  test("parse class selector"):
    val css = """
      |.fast {
      |  model: gemini-3-flash-preview;
      |}
    """.stripMargin
    val rules = StylesheetParser.parse(css)
    assert(rules.isRight)

  test("parse ID selector"):
    val css = """
      |#review {
      |  reasoning_effort: high;
      |  model: claude-opus-4-6;
      |}
    """.stripMargin
    val rules = StylesheetParser.parse(css)
    assert(rules.isRight)

  test("parse universal selector"):
    val css = """
      |* {
      |  model: gpt-5.2;
      |}
    """.stripMargin
    val rules = StylesheetParser.parse(css)
    assert(rules.isRight)

  test("specificity ordering: ID > class > shape > universal"):
    val css = """
      |* { model: default; }
      |box { model: shape; }
      |.fast { model: class; }
      |#review { model: id; }
    """.stripMargin
    val rules = StylesheetParser.parse(css)
    assert(rules.isRight)
    val ruleList = rules.toOption.get
    assertEquals(ruleList.length, 4)
    val sorted = ruleList.sortBy(_.selector.specificity)
    assertEquals(sorted(0).selector, Selector.Universal)
    assertEquals(sorted(1).selector, Selector.Shape("box"))
    assertEquals(sorted(2).selector, Selector.Class("fast"))
    assertEquals(sorted(3).selector, Selector.Id("review"))

  test("multiple properties in a rule"):
    val css = """
      |box {
      |  model: claude-opus-4-6;
      |  reasoning_effort: high;
      |  temperature: 0.7;
      |}
    """.stripMargin
    val rules = StylesheetParser.parse(css)
    assert(rules.isRight)
    val ruleList = rules.toOption.get
    assertEquals(ruleList.head.declarations.size, 3)

  test("equals syntax for declarations"):
    val css = """
      |box {
      |  model = "claude-opus-4-6"
      |  reasoning_effort = "high"
      |}
    """.stripMargin
    val rules = StylesheetParser.parse(css)
    assert(rules.isRight, s"Parse failed: $rules")
    val ruleList = rules.toOption.get
    assertEquals(ruleList.head.declarations("model"), "claude-opus-4-6")
    assertEquals(ruleList.head.declarations("reasoning_effort"), "high")

  test("empty stylesheet produces empty list"):
    val rules = StylesheetParser.parse("")
    assert(rules.isRight)
    assertEquals(rules.toOption.get.length, 0)
