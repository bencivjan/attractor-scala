package ai.attractor.pipeline.parser

import munit.FunSuite

class DotParserSuite extends FunSuite:

  test("parse simple linear pipeline"):
    val dot = """
      |digraph simple {
      |  start [shape=Mdiamond]
      |  step1 [shape=box, prompt="Do something"]
      |  done [shape=Msquare]
      |  start -> step1
      |  step1 -> done
      |}
    """.stripMargin
    val result = DotParser.parse(dot)
    assert(result.isRight, s"Parse failed: $result")
    val graph = result.toOption.get
    assertEquals(graph.name, "simple")
    assertEquals(graph.nodes.size, 3)
    assertEquals(graph.edges.size, 2)

  test("parse graph-level attributes"):
    val dot = """
      |digraph mypipeline {
      |  goal = "Build a feature"
      |  label = "Feature Pipeline"
      |  start [shape=Mdiamond]
      |  done [shape=Msquare]
      |  start -> done
      |}
    """.stripMargin
    val result = DotParser.parse(dot)
    assert(result.isRight)
    val graph = result.toOption.get
    assertEquals(graph.goal, "Build a feature")
    assertEquals(graph.label, "Feature Pipeline")

  test("parse chained edges"):
    val dot = """
      |digraph chain {
      |  A [shape=Mdiamond]
      |  B [shape=box]
      |  C [shape=box]
      |  D [shape=Msquare]
      |  A -> B -> C -> D
      |}
    """.stripMargin
    val result = DotParser.parse(dot)
    assert(result.isRight)
    val graph = result.toOption.get
    assertEquals(graph.edges.size, 3)
    assertEquals(graph.edges.map(e => (e.from, e.to)),
      List(("A", "B"), ("B", "C"), ("C", "D")))

  test("parse edge attributes"):
    val dot = """
      |digraph edges {
      |  start [shape=Mdiamond]
      |  A [shape=box]
      |  B [shape=box]
      |  done [shape=Msquare]
      |  start -> A [label="success", weight=2]
      |  start -> B [label="fail", condition="outcome = fail"]
      |  A -> done
      |  B -> done
      |}
    """.stripMargin
    val result = DotParser.parse(dot)
    assert(result.isRight)
    val graph = result.toOption.get
    val startEdges = graph.outgoingEdges("start")
    assertEquals(startEdges.size, 2)
    val successEdge = startEdges.find(_.label == "success")
    assert(successEdge.isDefined)
    assertEquals(successEdge.get.weight, 2)

  test("parse node defaults"):
    val dot = """
      |digraph defaults {
      |  node [shape=box]
      |  start [shape=Mdiamond]
      |  A
      |  B
      |  done [shape=Msquare]
      |  start -> A -> B -> done
      |}
    """.stripMargin
    val result = DotParser.parse(dot)
    assert(result.isRight)
    val graph = result.toOption.get
    assertEquals(graph.nodes("A").shape, "box")
    assertEquals(graph.nodes("B").shape, "box")

  test("strip line comments"):
    val dot = """
      |digraph comments {
      |  // This is a comment
      |  start [shape=Mdiamond]
      |  done [shape=Msquare] // inline comment
      |  start -> done
      |}
    """.stripMargin
    val result = DotParser.parse(dot)
    assert(result.isRight)

  test("strip block comments"):
    val dot = """
      |digraph blockcomments {
      |  /* multi
      |     line
      |     comment */
      |  start [shape=Mdiamond]
      |  done [shape=Msquare]
      |  start -> done
      |}
    """.stripMargin
    val result = DotParser.parse(dot)
    assert(result.isRight)

  test("parse quoted attribute values"):
    val dot = """
      |digraph quoted {
      |  start [shape=Mdiamond]
      |  step [shape=box, prompt="Do something\nwith newlines"]
      |  done [shape=Msquare]
      |  start -> step -> done
      |}
    """.stripMargin
    val result = DotParser.parse(dot)
    assert(result.isRight)
    val graph = result.toOption.get
    assert(graph.nodes("step").prompt.contains("\n"))

  test("parse subgraph"):
    val dot = """
      |digraph withsub {
      |  start [shape=Mdiamond]
      |  done [shape=Msquare]
      |  subgraph cluster_1 {
      |    A [shape=box]
      |    B [shape=box]
      |    A -> B
      |  }
      |  start -> A
      |  B -> done
      |}
    """.stripMargin
    val result = DotParser.parse(dot)
    assert(result.isRight)
    val graph = result.toOption.get
    assert(graph.nodes.contains("A"))
    assert(graph.nodes.contains("B"))

  test("edge weight default is 1"):
    val dot = """
      |digraph w {
      |  A [shape=Mdiamond]
      |  B [shape=Msquare]
      |  A -> B
      |}
    """.stripMargin
    val result = DotParser.parse(dot)
    val graph = result.toOption.get
    assertEquals(graph.edges.head.weight, 1)

  test("nodes from edges are auto-created"):
    val dot = """
      |digraph auto {
      |  A -> B -> C
      |}
    """.stripMargin
    val result = DotParser.parse(dot)
    assert(result.isRight)
    val graph = result.toOption.get
    assert(graph.nodes.contains("A"))
    assert(graph.nodes.contains("B"))
    assert(graph.nodes.contains("C"))
