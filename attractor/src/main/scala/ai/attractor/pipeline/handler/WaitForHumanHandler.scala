package ai.attractor.pipeline.handler

import cats.effect.IO

import ai.attractor.pipeline.parser.{Graph, Node}
import ai.attractor.pipeline.state.{Context, Outcome}
import ai.attractor.pipeline.interviewer.*

// ---------------------------------------------------------------------------
// Human-in-the-loop handler
// ---------------------------------------------------------------------------

/** Presents a choice derived from outgoing edges to a human reviewer
  * via the Interviewer abstraction.
  *
  * Edge labels are parsed for accelerator keys using these conventions:
  *   - `[K] Label`   -> key = "K"
  *   - `K) Label`    -> key = "K"
  *   - `K - Label`   -> key = "K"
  *   - Otherwise     -> key = first character of the label (uppercased)
  */
class WaitForHumanHandler(interviewer: Interviewer) extends Handler:

  def execute(node: Node, context: Context, graph: Graph, logsRoot: String): IO[Outcome] =
    val outEdges = graph.outgoingEdges(node.id)
    val choices = outEdges.flatMap: edge =>
      val raw = edge.label.trim
      if raw.isEmpty then None
      else Some(parseAccelerator(raw) -> edge.to)

    val options = choices.map: (opt, _) =>
      opt

    val question = Question(
      text = node.label,
      questionType =
        if options.size <= 2 && options.forall(o => Set("y", "n", "yes", "no").contains(o.key.toLowerCase))
        then QuestionType.YesNo
        else QuestionType.MultipleChoice,
      options = options,
      stage = node.id
    )

    for
      answer <- interviewer.ask(question)
    yield
      val selectedKey = answer.value
      val selectedLabel = answer.selectedOption.map(_.label).getOrElse(answer.text)

      // Map the selected key back to the target node id
      val nextIds = choices.collect:
        case (opt, targetId) if opt.key.equalsIgnoreCase(selectedKey) => targetId

      Outcome.success(
        notes = s"Human selected: $selectedLabel",
        contextUpdates = Map(
          "human.gate.selected" -> selectedKey,
          "human.gate.label" -> selectedLabel
        ),
        suggestedNextIds = nextIds,
        preferredLabel = selectedLabel
      )

  /** Parse an accelerator key from an edge label.
    *
    * Supported formats:
    *   - `[K] Label`
    *   - `K) Label`
    *   - `K - Label`
    *   - Bare label -> first character as key
    */
  private def parseAccelerator(raw: String): InterviewOption =
    val bracketPattern = """\[(\w+)\]\s*(.+)""".r
    val parenPattern   = """(\w+)\)\s*(.+)""".r
    val dashPattern    = """(\w+)\s*-\s*(.+)""".r

    raw match
      case bracketPattern(key, label) => InterviewOption(key.toUpperCase, label.trim)
      case parenPattern(key, label)   => InterviewOption(key.toUpperCase, label.trim)
      case dashPattern(key, label)    => InterviewOption(key.toUpperCase, label.trim)
      case other =>
        val key = other.headOption.map(_.toUpper.toString).getOrElse("?")
        InterviewOption(key, other.trim)
