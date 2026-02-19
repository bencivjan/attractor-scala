package ai.attractor.pipeline.pipelines

import munit.FunSuite
import ai.attractor.pipeline.parser.DotParser
import ai.attractor.pipeline.validation.{Validator, Severity}

class PipelinesSuite extends FunSuite:

  test("default pipeline is not empty") {
    val dot = Pipelines.default
    assert(dot.nonEmpty, "default pipeline returned empty string")
    assert(dot.contains("plan_build_verify"), "default pipeline should contain graph name")
  }

  test("default pipeline parses") {
    val result = DotParser.parse(Pipelines.default)
    assert(result.isRight, s"default pipeline failed to parse: ${result.left.getOrElse("")}")
    val graph = result.toOption.get
    assertEquals(graph.name, "plan_build_verify")
  }

  test("default pipeline has expected nodes") {
    val graph = DotParser.parse(Pipelines.default).toOption.get
    val expectedNodes = List(
      "start", "high_level_plan", "sprint_breakdown", "implement", "qa_verify",
      "submit_for_evaluation", "evaluation_feedback", "exit"
    )
    for id <- expectedNodes do
      assert(graph.nodes.contains(id), s"expected node '$id' not found in graph")
  }

  test("default pipeline model assignments") {
    val graph = DotParser.parse(Pipelines.default).toOption.get

    val expected = List(
      ("high_level_plan", "claude-opus-4-6", "anthropic"),
      ("sprint_breakdown", "claude-opus-4-6", "anthropic"),
      ("implement", "gpt-5.3-codex", "openai"),
      ("qa_verify", "claude-opus-4-6", "anthropic")
    )

    for (nodeId, model, provider) <- expected do
      val node = graph.nodes(nodeId)
      assertEquals(node.model, Some(model), s"node '$nodeId' model mismatch")
      assertEquals(node.llmProvider, Some(provider), s"node '$nodeId' provider mismatch")
  }

  test("default pipeline goal gates") {
    val graph = DotParser.parse(Pipelines.default).toOption.get
    val goalGateNodes = List("high_level_plan", "sprint_breakdown", "implement", "qa_verify")
    for id <- goalGateNodes do
      assert(graph.nodes(id).goalGate, s"expected node '$id' to be a goal gate")
  }

  test("default pipeline validates without errors") {
    val graph = DotParser.parse(Pipelines.default).toOption.get
    val diags = Validator.validate(graph)
    // Communication nodes (inbound ports) are intentionally unreachable in standalone mode
    val commNodeIds = graph.nodes.values.filter(_.nodeType == "communication").map(_.id).toSet
    val errors = diags.filter(d =>
      d.severity == Severity.Error && !d.nodeId.exists(commNodeIds.contains)
    )
    assert(errors.isEmpty, s"default pipeline has validation errors: ${errors.map(_.message).mkString("; ")}")
  }

  test("default pipeline edge structure") {
    val graph = DotParser.parse(Pipelines.default).toOption.get

    val expectedEdges = List(
      ("start", "high_level_plan"),
      ("high_level_plan", "sprint_breakdown"),
      ("sprint_breakdown", "implement"),
      ("implement", "qa_verify"),
      ("submit_for_evaluation", "exit"),
      ("evaluation_feedback", "implement")
    )

    for (from, to) <- expectedEdges do
      val found = graph.edges.exists(e => e.from == from && e.to == to)
      assert(found, s"expected edge $from -> $to not found")

    // QA pass -> submit_for_evaluation
    val qaToSubmit = graph.edges.exists(e => e.from == "qa_verify" && e.to == "submit_for_evaluation")
    assert(qaToSubmit, "expected qa_verify -> submit_for_evaluation edge")

    // QA fail -> implement (loop restart)
    val qaToImplement = graph.edges.filter(e => e.from == "qa_verify" && e.to == "implement")
    assert(qaToImplement.nonEmpty, "expected qa_verify -> implement feedback edge")
    assert(qaToImplement.head.loopRestart, "qa_verify -> implement edge should have loop_restart=true")
  }

  test("default pipeline communication nodes") {
    val graph = DotParser.parse(Pipelines.default).toOption.get

    val submit = graph.nodes("submit_for_evaluation")
    assertEquals(submit.nodeType, "communication")
    assertEquals(submit.attributes.get("direction"), Some("outbound"))

    val feedback = graph.nodes("evaluation_feedback")
    assertEquals(feedback.nodeType, "communication")
    assertEquals(feedback.attributes.get("direction"), Some("inbound"))
  }

  test("evaluator pipeline parses and validates") {
    val dot = Pipelines.get("evaluator")
    assert(dot.isDefined, "evaluator pipeline should be registered")
    val graph = DotParser.parse(dot.get).toOption.get
    assertEquals(graph.name, "evaluator")

    val diags = Validator.validate(graph)
    val errors = diags.filter(_.severity == Severity.Error)
    assert(errors.isEmpty, s"evaluator pipeline has validation errors: ${errors.map(_.message).mkString("; ")}")
  }

  test("evaluator pipeline has expected nodes") {
    val graph = DotParser.parse(Pipelines.get("evaluator").get).toOption.get
    val expectedNodes = List(
      "start", "receive_submission", "orchestrator", "builder", "qa",
      "visionary", "return_feedback", "exit"
    )
    for id <- expectedNodes do
      assert(graph.nodes.contains(id), s"expected node '$id' not found in evaluator graph")
  }

  test("evaluator pipeline edge structure with retry loop") {
    val graph = DotParser.parse(Pipelines.get("evaluator").get).toOption.get

    // Main flow
    val expectedEdges = List(
      ("start", "receive_submission"),
      ("receive_submission", "orchestrator"),
      ("orchestrator", "builder"),
      ("builder", "qa"),
      ("qa", "visionary"),
      ("return_feedback", "exit")
    )
    for (from, to) <- expectedEdges do
      val found = graph.edges.exists(e => e.from == from && e.to == to)
      assert(found, s"expected edge $from -> $to not found")

    // Visionary -> exit (approval)
    val visionaryToExit = graph.edges.filter(e => e.from == "visionary" && e.to == "exit")
    assert(visionaryToExit.nonEmpty, "expected visionary -> exit edge")

    // Visionary -> orchestrator (retry loop for insufficient evaluation)
    val visionaryToOrchestrator = graph.edges.filter(e => e.from == "visionary" && e.to == "orchestrator")
    assert(visionaryToOrchestrator.nonEmpty, "expected visionary -> orchestrator retry edge")
    assertEquals(visionaryToOrchestrator.head.condition, "outcome=retry")

    // Visionary -> return_feedback (rejection)
    val visionaryToFeedback = graph.edges.filter(e => e.from == "visionary" && e.to == "return_feedback")
    assert(visionaryToFeedback.nonEmpty, "expected visionary -> return_feedback edge")
    assertEquals(visionaryToFeedback.head.condition, "outcome=fail")
  }

  test("evaluator pipeline communication nodes") {
    val graph = DotParser.parse(Pipelines.get("evaluator").get).toOption.get

    val receive = graph.nodes("receive_submission")
    assertEquals(receive.nodeType, "communication")
    assertEquals(receive.attributes.get("direction"), Some("inbound"))

    val feedback = graph.nodes("return_feedback")
    assertEquals(feedback.nodeType, "communication")
    assertEquals(feedback.attributes.get("direction"), Some("outbound"))
  }

  test("factory pipeline parses and validates") {
    val dot = Pipelines.get("factory")
    assert(dot.isDefined, "factory pipeline should be registered")
    val graph = DotParser.parse(dot.get).toOption.get
    assertEquals(graph.name, "factory")

    val diags = Validator.validate(graph)
    val errors = diags.filter(_.severity == Severity.Error)
    assert(errors.isEmpty, s"factory pipeline has validation errors: ${errors.map(_.message).mkString("; ")}")
  }

  test("factory pipeline has all developer and evaluator nodes") {
    val graph = DotParser.parse(Pipelines.get("factory").get).toOption.get
    val expectedNodes = List(
      "start", "high_level_plan", "sprint_breakdown", "implement", "qa_verify",
      "eval_orchestrator", "eval_builder", "eval_qa", "eval_visionary", "exit"
    )
    for id <- expectedNodes do
      assert(graph.nodes.contains(id), s"expected node '$id' not found in factory graph")
  }

  test("factory pipeline cross-phase edges") {
    val graph = DotParser.parse(Pipelines.get("factory").get).toOption.get

    // QA pass hands off to evaluator
    val qaToEvalOrch = graph.edges.exists(e => e.from == "qa_verify" && e.to == "eval_orchestrator")
    assert(qaToEvalOrch, "expected qa_verify -> eval_orchestrator handoff edge")

    // Eval visionary rejection feeds back to developer implementation
    val evalVisToImpl = graph.edges.filter(e => e.from == "eval_visionary" && e.to == "implement")
    assert(evalVisToImpl.nonEmpty, "expected eval_visionary -> implement rejection edge")
    assertEquals(evalVisToImpl.head.condition, "outcome=fail")

    // Eval visionary retry loops within evaluator
    val evalVisToOrch = graph.edges.filter(e => e.from == "eval_visionary" && e.to == "eval_orchestrator")
    assert(evalVisToOrch.nonEmpty, "expected eval_visionary -> eval_orchestrator retry edge")
    assertEquals(evalVisToOrch.head.condition, "outcome=retry")
  }

  test("get returns None for unknown pipeline") {
    assertEquals(Pipelines.get("nonexistent_pipeline"), None)
  }

  test("register adds pipeline to catalog") {
    Pipelines.register("test_pipeline", "digraph test { start [shape=Mdiamond]; exit [shape=Msquare]; start -> exit }")
    val dot = Pipelines.get("test_pipeline")
    assert(dot.isDefined, "registered pipeline should be retrievable")
  }

  test("names includes built-in pipelines") {
    val allNames = Pipelines.names
    assert(allNames.contains(Pipelines.DefaultName), s"names should include '${Pipelines.DefaultName}'")
    assert(allNames.contains("evaluator"), "names should include 'evaluator'")
    assert(allNames.contains("factory"), "names should include 'factory'")
  }
