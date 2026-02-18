package ai.attractor.pipeline.condition

import munit.CatsEffectSuite
import cats.effect.IO
import ai.attractor.pipeline.state.{Context, Outcome, StageStatus}

class ConditionEvaluatorSuite extends CatsEffectSuite:

  test("empty condition evaluates to true"):
    for
      context <- Context.of(Map.empty)
      result <- ConditionEvaluator.evaluateCondition("", Outcome.success(), context)
    yield assert(result)

  test("outcome = success matches success outcome"):
    for
      context <- Context.of(Map.empty)
      result <- ConditionEvaluator.evaluateCondition("outcome = success", Outcome.success(), context)
    yield assert(result)

  test("outcome = fail matches fail outcome"):
    for
      context <- Context.of(Map.empty)
      result <- ConditionEvaluator.evaluateCondition("outcome = fail", Outcome.fail(), context)
    yield assert(result)

  test("outcome != fail does not match fail outcome"):
    for
      context <- Context.of(Map.empty)
      result <- ConditionEvaluator.evaluateCondition("outcome != fail", Outcome.fail(), context)
    yield assert(!result)

  test("preferred_label matches outcome label"):
    for
      context <- Context.of(Map.empty)
      outcome = Outcome.success(preferredLabel = "approve")
      result <- ConditionEvaluator.evaluateCondition("preferred_label = approve", outcome, context)
    yield assert(result)

  test("context variable lookup"):
    for
      context <- Context.of(Map("language" -> "scala"))
      result <- ConditionEvaluator.evaluateCondition("context.language = scala", Outcome.success(), context)
    yield assert(result)

  test("AND conjunction works"):
    for
      context <- Context.of(Map("ready" -> "true"))
      outcome = Outcome.success()
      result <- ConditionEvaluator.evaluateCondition("outcome = success && context.ready = true", outcome, context)
    yield assert(result)

  test("AND conjunction fails when one clause fails"):
    for
      context <- Context.of(Map("ready" -> "false"))
      outcome = Outcome.success()
      result <- ConditionEvaluator.evaluateCondition("outcome = success && context.ready = true", outcome, context)
    yield assert(!result)

  test("missing context variable does not match a value"):
    for
      context <- Context.of(Map.empty)
      result <- ConditionEvaluator.evaluateCondition("context.missing != something", Outcome.success(), context)
    yield assert(result)

  test("status.* prefix resolves context status"):
    for
      context <- Context.of(Map("status.step1" -> "success"))
      result <- ConditionEvaluator.evaluateCondition("status.step1 = success", Outcome.success(), context)
    yield assert(result)
