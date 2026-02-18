package ai.attractor.pipeline.stylesheet

import ai.attractor.pipeline.parser.{Graph, Node}

// ---------------------------------------------------------------------------
// CSS-like model stylesheet parser and applicator.
//
// Supported selector syntax:
//   *         -- universal, matches all nodes (specificity 0)
//   box       -- shape selector, matches nodes by DOT shape (specificity 1)
//   .name     -- class selector, matches nodes with that class (specificity 2)
//   #name     -- ID selector, matches a node by its id (specificity 3)
//
// Declaration syntax (both forms accepted):
//   property: value;
//   property = value
//
// Example stylesheet:
//   * { llm_model: claude-sonnet-4-5; }
//   box { model = "claude-opus-4-6"; reasoning_effort = "high" }
//   .code { llm_model: claude-opus-4-6; reasoning_effort: high; }
//   #review { reasoning_effort: max; }
//
// When applying, rules are sorted by specificity (lowest first) so that
// higher-specificity rules override lower ones.  Properties already set
// explicitly on a node are never overridden by stylesheet rules.
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// Selector ADT
// ---------------------------------------------------------------------------

enum Selector:
  case Universal
  case Shape(name: String)
  case Class(name: String)
  case Id(name: String)

  def specificity: Int = this match
    case Universal  => 0
    case Shape(_)   => 1
    case Class(_)   => 2
    case Id(_)      => 3

  def matches(node: Node): Boolean = this match
    case Universal    => true
    case Shape(name)  => node.shape.equalsIgnoreCase(name)
    case Class(name)  => node.classes.contains(name)
    case Id(name)     => node.id == name

// ---------------------------------------------------------------------------
// StyleRule
// ---------------------------------------------------------------------------

case class StyleRule(
    selector: Selector,
    declarations: Map[String, String]
)

// ---------------------------------------------------------------------------
// StylesheetParser
// ---------------------------------------------------------------------------

object StylesheetParser:

  /** Parse a stylesheet string into a list of style rules. */
  def parse(input: String): Either[String, List[StyleRule]] =
    val stripped = stripComments(input)
    parseRules(stripped.trim, Nil)

  /**
   * Apply stylesheet rules to all nodes in a graph.
   *
   * For each node, collect matching rules sorted by specificity (ascending),
   * merge their declarations, and only set properties that are not already
   * explicitly defined on the node.
   */
  def applyStylesheet(graph: Graph, rules: List[StyleRule]): Graph =
    val updatedNodes = graph.nodes.map: (id, node) =>
      val matchingRules = rules
        .filter(_.selector.matches(node))
        .sortBy(_.selector.specificity)

      // Merge declarations from low to high specificity
      val mergedDecls = matchingRules.foldLeft(Map.empty[String, String]): (acc, rule) =>
        acc ++ rule.declarations

      // Only apply declarations for keys not already explicit on the node
      val newAttrs = mergedDecls.foldLeft(node.attributes): (attrs, kv) =>
        val (key, value) = kv
        if attrs.contains(key) then attrs
        else attrs + (key -> value)

      id -> node.copy(attributes = newAttrs)

    graph.copy(nodes = updatedNodes)

  /**
   * Convenience: parse a stylesheet string and apply it to a graph.
   * Returns Left if the stylesheet has syntax errors.
   */
  def parseAndApply(graph: Graph, stylesheet: String): Either[String, Graph] =
    parse(stylesheet).map(rules => applyStylesheet(graph, rules))

  // -------------------------------------------------------------------------
  // Internal parsing
  // -------------------------------------------------------------------------

  /** Strip C-style block comments and line comments. */
  private def stripComments(src: String): String =
    val sb = StringBuilder()
    var i = 0
    while i < src.length do
      if i + 1 < src.length && src(i) == '/' && src(i + 1) == '/' then
        i += 2
        while i < src.length && src(i) != '\n' do i += 1
      else if i + 1 < src.length && src(i) == '/' && src(i + 1) == '*' then
        i += 2
        while i + 1 < src.length && !(src(i) == '*' && src(i + 1) == '/') do i += 1
        if i + 1 < src.length then i += 2
      else
        sb.append(src(i))
        i += 1
    sb.toString

  /** Recursively parse rules from the remaining input. */
  private def parseRules(
      remaining: String,
      acc: List[StyleRule]
  ): Either[String, List[StyleRule]] =
    val trimmed = remaining.trim
    if trimmed.isEmpty then Right(acc)
    else
      for
        (rule, rest) <- parseSingleRule(trimmed)
        result       <- parseRules(rest, acc :+ rule)
      yield result

  /** Parse one rule: selector { declarations } */
  private def parseSingleRule(input: String): Either[String, (StyleRule, String)] =
    val braceIdx = input.indexOf('{')
    if braceIdx < 0 then Left(s"Expected '{' in stylesheet rule near: '${input.take(40)}...'")
    else
      val selectorStr = input.substring(0, braceIdx).trim
      val afterBrace = input.substring(braceIdx + 1)
      val closeBraceIdx = afterBrace.indexOf('}')
      if closeBraceIdx < 0 then Left(s"Expected '}' to close rule for selector '$selectorStr'")
      else
        val declsStr = afterBrace.substring(0, closeBraceIdx).trim
        val rest = afterBrace.substring(closeBraceIdx + 1)
        for
          selector <- parseSelector(selectorStr)
          decls    <- parseDeclarations(declsStr)
        yield (StyleRule(selector, decls), rest)

  /** Parse a selector string into a Selector. */
  private def parseSelector(s: String): Either[String, Selector] =
    val trimmed = s.trim
    if trimmed == "*" then Right(Selector.Universal)
    else if trimmed.startsWith("#") then
      val name = trimmed.drop(1).trim
      if name.isEmpty then Left("Empty ID selector")
      else Right(Selector.Id(name))
    else if trimmed.startsWith(".") then
      val name = trimmed.drop(1).trim
      if name.isEmpty then Left("Empty class selector")
      else Right(Selector.Class(name))
    else if trimmed.matches("[a-zA-Z][a-zA-Z0-9_]*") then
      // Bare word is a shape selector (e.g., box, diamond, Mdiamond)
      Right(Selector.Shape(trimmed))
    else
      Left(s"Unrecognized selector: '$trimmed' (expected *, shape, .class, or #id)")

  /**
   * Parse declarations supporting both CSS-style (key: value;) and
   * DOT-style (key = "value") syntax. Declarations can be separated
   * by semicolons or newlines.
   */
  private def parseDeclarations(s: String): Either[String, Map[String, String]] =
    val trimmed = s.trim
    if trimmed.isEmpty then Right(Map.empty)
    else
      // Split on semicolons or newlines, then filter empties
      val pairs = trimmed.split("[;\n]").toList.map(_.trim).filter(_.nonEmpty)
      val result = pairs.foldLeft[Either[String, Map[String, String]]](Right(Map.empty)):
        case (Left(err), _) => Left(err)
        case (Right(acc), pair) =>
          findSeparator(pair) match
            case Some(sepIdx) =>
              val key = pair.substring(0, sepIdx).trim
              val rawValue = pair.substring(sepIdx + 1).trim
              val value = unquote(rawValue)
              if key.isEmpty then Left(s"Empty property name in declaration: '$pair'")
              else Right(acc + (key -> value))
            case None =>
              Left(s"Expected ':' or '=' in declaration: '$pair'")
      result

  /** Find the index of the separator (`:` or `=`) in a declaration. */
  private def findSeparator(pair: String): Option[Int] =
    val colonIdx = pair.indexOf(':')
    val eqIdx = pair.indexOf('=')
    (colonIdx, eqIdx) match
      case (c, e) if c >= 0 && e >= 0 => Some(Math.min(c, e))
      case (c, _) if c >= 0           => Some(c)
      case (_, e) if e >= 0           => Some(e)
      case _                          => None

  /** Strip surrounding quotes from a value if present. */
  private def unquote(s: String): String =
    if s.length >= 2 && s.startsWith("\"") && s.endsWith("\"") then
      s.substring(1, s.length - 1)
    else s
