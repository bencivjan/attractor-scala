package ai.attractor.agent.tool

import ai.attractor.agent.session.SessionConfig

object Truncation:

  // Default character limits per tool
  private val defaultCharLimits: Map[String, Int] = Map(
    "read_file" -> 50000,
    "shell" -> 30000,
    "grep" -> 20000,
    "glob" -> 20000,
    "edit_file" -> 10000,
    "write_file" -> 1000
  )

  // Default line limits per tool
  private val defaultLineLimits: Map[String, Int] = Map(
    "shell" -> 256,
    "grep" -> 200,
    "glob" -> 500
  )

  private val defaultMaxChars: Int = 30000
  private val defaultMaxLines: Int = 500

  /** Truncate output by character count using the specified mode.
    *
    * @param output The output string to truncate
    * @param maxChars Maximum number of characters to keep
    * @param mode Either "head_tail" (keeps beginning and end) or "tail" (keeps end only)
    * @return Truncated string with a marker indicating truncation occurred
    */
  def truncateOutput(output: String, maxChars: Int, mode: String = "head_tail"): String =
    if output.length <= maxChars then output
    else
      val removed = output.length - maxChars
      mode match
        case "tail" =>
          val kept = output.takeRight(maxChars)
          s"[WARNING: Tool output was truncated. $removed characters removed from beginning.]\n$kept"

        case _ => // "head_tail" default
          val halfChars = maxChars / 2
          val head = output.take(halfChars)
          val tail = output.takeRight(halfChars)
          val omitted = output.length - halfChars * 2
          s"$head\n[WARNING: Tool output was truncated. $omitted characters removed.]\n$tail"

  /** Truncate output by line count, keeping first and last lines.
    *
    * @param output The output string to truncate
    * @param maxLines Maximum number of lines to keep
    * @return Truncated string with a marker indicating lines were omitted
    */
  def truncateLines(output: String, maxLines: Int): String =
    val lines = output.split("\n", -1)
    if lines.length <= maxLines then output
    else
      val halfLines = maxLines / 2
      val headLines = lines.take(halfLines)
      val tailLines = lines.takeRight(halfLines)
      val omitted = lines.length - halfLines * 2
      (headLines.toList ++ List(s"[WARNING: Tool output was truncated. $omitted lines removed.]") ++ tailLines.toList).mkString("\n")

  /** Apply both character and line truncation for a specific tool, using
    * config overrides or falling back to defaults.
    *
    * @param output The tool output to truncate
    * @param toolName The name of the tool that produced the output
    * @param config Session configuration with optional per-tool overrides
    * @return Truncated output
    */
  def truncateToolOutput(output: String, toolName: String, config: SessionConfig): String =
    val charLimit = config.toolOutputLimits
      .getOrElse(toolName, defaultCharLimits.getOrElse(toolName, defaultMaxChars))

    val lineLimit = config.toolLineLimits
      .getOrElse(toolName, defaultLineLimits.getOrElse(toolName, defaultMaxLines))

    // Apply character truncation FIRST (handles pathological cases),
    // then line truncation SECOND (as specified in the spec)
    val charTruncated = truncateOutput(output, charLimit)
    truncateLines(charTruncated, lineLimit)
