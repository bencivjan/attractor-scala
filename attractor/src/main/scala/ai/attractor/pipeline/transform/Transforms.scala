package ai.attractor.pipeline.transform

import ai.attractor.pipeline.parser.{Graph, Node}

// ---------------------------------------------------------------------------
// Pipeline graph transforms -- applied before execution
// ---------------------------------------------------------------------------

/** A transform modifies the pipeline graph before execution begins. */
trait Transform:
  def apply(graph: Graph): Graph

/** Expands `$goal` references in node prompts with the graph-level goal. */
object VariableExpansionTransform extends Transform:

  def apply(graph: Graph): Graph =
    val goal = graph.goal
    if goal.isEmpty then graph
    else
      val updatedNodes = graph.nodes.map: (id, node) =>
        val expandedAttrs = node.attributes.map: (k, v) =>
          k -> expandVariables(v, goal)
        id -> node.copy(attributes = expandedAttrs)
      graph.copy(nodes = updatedNodes)

  private def expandVariables(value: String, goal: String): String =
    value.replace("$goal", goal)

/** Applies `model_stylesheet` graph attribute values to matching nodes.
  *
  * The stylesheet format is a semicolon-separated list of rules:
  *   `selector:attribute=value;selector:attribute=value`
  *
  * Selectors:
  *   - `*`           -> all nodes
  *   - `.classname`  -> nodes with matching class
  *   - `#nodeId`     -> specific node by id
  *   - `shapename`   -> nodes with matching shape
  */
object StylesheetApplicationTransform extends Transform:

  def apply(graph: Graph): Graph =
    val stylesheet = graph.modelStylesheet
    if stylesheet.isEmpty then graph
    else
      val rules = parseStylesheet(stylesheet)
      val updatedNodes = graph.nodes.map: (id, node) =>
        val mergedAttrs = rules.foldLeft(node.attributes): (attrs, rule) =>
          if matchesSelector(node, rule.selector) then
            // Stylesheet attributes do not override explicit node attributes
            rule.assignments.foldLeft(attrs): (acc, entry) =>
              val key = entry._1
              val value = entry._2
              if acc.contains(key) then acc else acc + (key -> value)
          else attrs
        id -> node.copy(attributes = mergedAttrs)
      graph.copy(nodes = updatedNodes)

  private case class StyleRule(selector: String, assignments: Map[String, String])

  private def parseStylesheet(stylesheet: String): List[StyleRule] =
    stylesheet.split(";").toList.flatMap: segment =>
      val trimmed = segment.trim
      if trimmed.isEmpty then None
      else
        // Format: selector:key=value or selector:key=value,key2=value2
        val colonIdx = trimmed.indexOf(':')
        if colonIdx <= 0 then None
        else
          val selector = trimmed.substring(0, colonIdx).trim
          val assignmentStr = trimmed.substring(colonIdx + 1).trim
          val assignments = assignmentStr.split(",").toList.flatMap: pair =>
            val eqIdx = pair.indexOf('=')
            if eqIdx > 0 then
              Some(pair.substring(0, eqIdx).trim -> pair.substring(eqIdx + 1).trim)
            else None
          if assignments.nonEmpty then Some(StyleRule(selector, assignments.toMap))
          else None

  private def matchesSelector(node: Node, selector: String): Boolean =
    selector match
      case "*"                    => true
      case s if s.startsWith(".") => node.classes.contains(s.drop(1))
      case s if s.startsWith("#") => node.id == s.drop(1)
      case shape                  => node.shape == shape
