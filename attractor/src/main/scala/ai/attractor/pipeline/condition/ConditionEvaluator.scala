package ai.attractor.pipeline.condition

import cats.effect.IO
import ai.attractor.pipeline.state.{Outcome, Context}

// ---------------------------------------------------------------------------
// Condition expression evaluator.
//
// Grammar:
//   condition  ::= clause ( "&&" clause )*
//   clause     ::= key operator value
//   operator   ::= "=" | "!=" | "=="
//   key        ::= identifier ( "." identifier )*
//   value      ::= identifier | quoted-string
//
// Special resolution:
//   - "outcome"            -> outcome.status name (lowercase)
//   - "outcome.status"     -> outcome.status name (lowercase)
//   - "outcome.label"      -> outcome.preferredLabel
//   - "context.<key>"      -> context value as string
//   - bare key             -> context value as string (fallback)
//
// An empty condition always evaluates to true.
// ---------------------------------------------------------------------------

object ConditionEvaluator:

  /** Evaluate a condition expression against an outcome and context (IO). */
  def evaluateCondition(condition: String, outcome: Outcome, context: Context): IO[Boolean] =
    val trimmed = condition.trim
    if trimmed.isEmpty then IO.pure(true)
    else
      context.snapshot.map: snap =>
        val clauses = splitClauses(trimmed)
        clauses.forall(clause => evaluateClauseSync(clause.trim, outcome, snap))

  /** Evaluate a single clause using a context snapshot (IO). */
  def evaluateClause(clause: String, outcome: Outcome, context: Context): IO[Boolean] =
    context.snapshot.map: snap =>
      evaluateClauseSync(clause, outcome, snap)

  /** Resolve a dotted key to its string value (IO). */
  def resolveKey(key: String, outcome: Outcome, context: Context): IO[String] =
    context.snapshot.map: snap =>
      resolveKeySync(key, outcome, snap)

  /**
   * Validate condition syntax without evaluating.
   * Returns Right(()) if valid, Left(error) if malformed.
   */
  def validateSyntax(condition: String): Either[String, Unit] =
    val trimmed = condition.trim
    if trimmed.isEmpty then Right(())
    else
      val clauses = splitClauses(trimmed)
      val errors = clauses.flatMap: clause =>
        parseClause(clause.trim) match
          case Left(err) => List(err)
          case Right(_)  => Nil
      if errors.isEmpty then Right(())
      else Left(errors.mkString("; "))

  // -------------------------------------------------------------------------
  // Synchronous evaluation (works on context snapshot)
  // -------------------------------------------------------------------------

  /** Evaluate a single clause synchronously against a context snapshot. */
  private[condition] def evaluateClauseSync(
      clause: String,
      outcome: Outcome,
      contextSnap: Map[String, Any]
  ): Boolean =
    parseClause(clause) match
      case Left(_) => false
      case Right((key, op, expected)) =>
        val actual = resolveKeySync(key, outcome, contextSnap)
        op match
          case "="  => actual.equalsIgnoreCase(expected)
          case "!=" => !actual.equalsIgnoreCase(expected)
          case _    => false

  /** Resolve a dotted key synchronously against a context snapshot. */
  private[condition] def resolveKeySync(
      key: String,
      outcome: Outcome,
      contextSnap: Map[String, Any]
  ): String =
    key match
      case "outcome" | "outcome.status" =>
        outcome.status.toString.toLowerCase
      case "outcome.label" | "preferred_label" =>
        outcome.preferredLabel
      case "outcome.notes" =>
        outcome.notes
      case "outcome.failure_reason" =>
        outcome.failureReason
      case k if k.startsWith("context.") =>
        val contextKey = k.stripPrefix("context.")
        contextSnap.get(contextKey).map(_.toString).getOrElse("")
      case k if k.startsWith("status.") =>
        // Allow querying status of any node via status.<node_id>
        contextSnap.get(k).map(_.toString).getOrElse("")
      case k =>
        // Fallback: look up in context
        contextSnap.get(k).map(_.toString).getOrElse("")

  // -------------------------------------------------------------------------
  // Internal helpers
  // -------------------------------------------------------------------------

  /** Split condition on `&&` boundaries, respecting quoted strings. */
  private def splitClauses(condition: String): List[String] =
    val parts = scala.collection.mutable.ListBuffer[String]()
    val current = StringBuilder()
    var i = 0
    var inQuote = false

    while i < condition.length do
      if inQuote then
        if condition(i) == '\\' && i + 1 < condition.length then
          current.append(condition(i))
          current.append(condition(i + 1))
          i += 2
        else
          if condition(i) == '"' then inQuote = false
          current.append(condition(i))
          i += 1
      else if condition(i) == '"' then
        inQuote = true
        current.append(condition(i))
        i += 1
      else if i + 1 < condition.length && condition(i) == '&' && condition(i + 1) == '&' then
        parts += current.toString
        current.clear()
        i += 2
      else
        current.append(condition(i))
        i += 1

    parts += current.toString
    parts.toList

  /** Parse a single clause into (key, operator, expectedValue). */
  private def parseClause(clause: String): Either[String, (String, String, String)] =
    val trimmed = clause.trim
    // Try != first (longer operator)
    val neqIdx = trimmed.indexOf("!=")
    if neqIdx > 0 then
      val key = trimmed.substring(0, neqIdx).trim
      val value = unquote(trimmed.substring(neqIdx + 2).trim)
      if key.isEmpty || value.isEmpty then Left(s"Malformed clause: '$trimmed'")
      else Right((key, "!=", value))
    else
      // Support both = and == as equality operators
      val doubleEqIdx = trimmed.indexOf("==")
      val eqIdx = if doubleEqIdx > 0 then doubleEqIdx else trimmed.indexOf('=')
      if eqIdx > 0 then
        val key = trimmed.substring(0, eqIdx).trim
        val opLen = if doubleEqIdx == eqIdx then 2 else 1
        val value = unquote(trimmed.substring(eqIdx + opLen).trim)
        if key.isEmpty || value.isEmpty then Left(s"Malformed clause: '$trimmed'")
        else Right((key, "=", value))
      else
        Left(s"Clause missing operator (= or !=): '$trimmed'")

  /** Strip surrounding quotes from a value if present. */
  private def unquote(s: String): String =
    if s.length >= 2 && s.startsWith("\"") && s.endsWith("\"") then
      s.substring(1, s.length - 1)
    else s
