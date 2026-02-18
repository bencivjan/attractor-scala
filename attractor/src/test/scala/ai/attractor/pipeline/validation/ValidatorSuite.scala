package ai.attractor.pipeline.validation

import munit.FunSuite
import ai.attractor.pipeline.parser.{DotParser, Graph, Node, Edge}

class ValidatorSuite extends FunSuite:

  private def parseAndValidate(dot: String): List[Diagnostic] =
    DotParser.parse(dot) match
      case Right(graph) => Validator.validate(graph)
      case Left(err) => fail(s"Parse failed: $err")

  test("valid pipeline produces no errors"):
    val dot = """
      |digraph valid {
      |  start [shape=Mdiamond]
      |  step [shape=box, prompt="Do it"]
      |  done [shape=Msquare]
      |  start -> step -> done
      |}
    """.stripMargin
    val diags = parseAndValidate(dot)
    val errors = diags.filter(_.severity == Severity.Error)
    assert(errors.isEmpty, s"Unexpected errors: $errors")

  test("missing start node produces error"):
    val graph = Graph(
      "test",
      Map("done" -> Node("done", Map("shape" -> "Msquare"))),
      List.empty
    )
    val diags = Validator.validate(graph)
    assert(diags.exists(d => d.severity == Severity.Error && d.rule == "start_node"))

  test("missing exit node produces error"):
    val graph = Graph(
      "test",
      Map("start" -> Node("start", Map("shape" -> "Mdiamond"))),
      List.empty
    )
    val diags = Validator.validate(graph)
    assert(diags.exists(d => d.severity == Severity.Error && d.rule == "terminal_node"))

  test("orphan node produces warning"):
    val dot = """
      |digraph orphan {
      |  start [shape=Mdiamond]
      |  orphan [shape=box]
      |  done [shape=Msquare]
      |  start -> done
      |}
    """.stripMargin
    val diags = parseAndValidate(dot)
    assert(diags.exists(_.rule == "reachability"))

  test("edge to nonexistent node produces error"):
    val graph = Graph(
      "test",
      Map(
        "start" -> Node("start", Map("shape" -> "Mdiamond")),
        "done" -> Node("done", Map("shape" -> "Msquare"))
      ),
      List(Edge("start", "missing"))
    )
    val diags = Validator.validate(graph)
    assert(diags.exists(d => d.severity == Severity.Error && d.rule == "edge_target_exists"))

  test("start node with incoming edges produces error"):
    val graph = Graph(
      "test",
      Map(
        "start" -> Node("start", Map("shape" -> "Mdiamond")),
        "step" -> Node("step", Map("shape" -> "box")),
        "done" -> Node("done", Map("shape" -> "Msquare"))
      ),
      List(
        Edge("step", "start"),
        Edge("start", "step"),
        Edge("step", "done")
      )
    )
    val diags = Validator.validate(graph)
    assert(diags.exists(d => d.severity == Severity.Error && d.rule == "start_no_incoming"))

  test("validateOrRaise returns Left for invalid graph"):
    val graph = Graph("empty", Map.empty, List.empty)
    val result = Validator.validateOrRaise(graph)
    assert(result.isLeft)

  test("validateOrRaise returns Right for valid graph"):
    val dot = """
      |digraph valid {
      |  start [shape=Mdiamond]
      |  step [shape=box, prompt="work"]
      |  done [shape=Msquare]
      |  start -> step -> done
      |}
    """.stripMargin
    DotParser.parse(dot) match
      case Right(graph) =>
        val result = Validator.validateOrRaise(graph)
        assert(result.isRight)
      case Left(err) => fail(s"Parse failed: $err")
