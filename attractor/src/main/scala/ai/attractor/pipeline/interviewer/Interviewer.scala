package ai.attractor.pipeline.interviewer

import cats.effect.IO
import cats.effect.std.Queue
import cats.syntax.all.*

// ---------------------------------------------------------------------------
// Interviewer abstraction for human-in-the-loop interactions
// ---------------------------------------------------------------------------

enum QuestionType:
  case SingleSelect, MultiSelect, FreeText, Confirm
  // Aliases for backwards compatibility
  case YesNo, MultipleChoice, Freeform, Confirmation

/** A selectable option presented to the user. */
case class InterviewOption(key: String, label: String)

/** A question to pose to a human (or automated) reviewer. */
case class Question(
    text: String,
    questionType: QuestionType,
    options: List[InterviewOption] = Nil,
    default: Option[Answer] = None,
    timeoutSeconds: Option[Double] = None,
    stage: String = "",
    metadata: Map[String, String] = Map.empty
)

/** Sentinel values for non-textual answers. */
enum AnswerValue:
  case Yes, No, Skipped, Timeout

/** An answer from a human (or automated) reviewer. */
case class Answer(
    value: String = "",
    selectedOption: Option[InterviewOption] = None,
    text: String = ""
)

object Answer:
  val yes: Answer     = Answer(value = AnswerValue.Yes.toString)
  val no: Answer      = Answer(value = AnswerValue.No.toString)
  val skipped: Answer = Answer(value = AnswerValue.Skipped.toString)
  val timeout: Answer = Answer(value = AnswerValue.Timeout.toString)

/** Core interviewer trait for human-in-the-loop pipeline stages. */
trait Interviewer:
  def ask(question: Question): IO[Answer]

  def askMultiple(questions: List[Question]): IO[List[Answer]] =
    questions.traverse(ask)

  def inform(message: String, stage: String): IO[Unit]

// ---------------------------------------------------------------------------
// AutoApproveInterviewer -- always approves / picks first option
// ---------------------------------------------------------------------------

object AutoApproveInterviewer extends Interviewer:
  def ask(question: Question): IO[Answer] = IO.pure:
    question.questionType match
      case QuestionType.YesNo | QuestionType.Confirmation | QuestionType.Confirm =>
        Answer.yes
      case QuestionType.MultipleChoice | QuestionType.SingleSelect =>
        question.options.headOption match
          case Some(opt) => Answer(value = opt.key, selectedOption = Some(opt), text = opt.label)
          case None      => Answer.yes
      case QuestionType.MultiSelect =>
        // Select all options for multi-select
        val labels = question.options.map(_.label).mkString(", ")
        Answer(value = labels, text = labels)
      case QuestionType.Freeform | QuestionType.FreeText =>
        question.default.getOrElse(Answer(value = "approved", text = "auto-approved"))

  def inform(message: String, stage: String): IO[Unit] = IO.unit

// ---------------------------------------------------------------------------
// ConsoleInterviewer -- reads from stdin
// ---------------------------------------------------------------------------

object ConsoleInterviewer extends Interviewer:
  def ask(question: Question): IO[Answer] = IO.blocking:
    System.out.println(s"\n${question.text}")
    question.options.foreach: opt =>
      System.out.println(s"  [${opt.key}] ${opt.label}")
    System.out.print("> ")
    System.out.flush()
    val input = scala.io.StdIn.readLine().trim
    if input.isEmpty then
      question.default.getOrElse(Answer(value = input, text = input))
    else
      val matched = question.options.find: opt =>
        opt.key.equalsIgnoreCase(input) || opt.label.equalsIgnoreCase(input)
      matched match
        case Some(opt) => Answer(value = opt.key, selectedOption = Some(opt), text = opt.label)
        case None      => Answer(value = input, text = input)

  def inform(message: String, stage: String): IO[Unit] = IO.blocking:
    System.out.println(s"[$stage] $message")

// ---------------------------------------------------------------------------
// CallbackInterviewer -- delegates to a provided function
// ---------------------------------------------------------------------------

class CallbackInterviewer(
    callback: Question => IO[Answer],
    onInform: (String, String) => IO[Unit] = (_, _) => IO.unit
) extends Interviewer:
  def ask(question: Question): IO[Answer] = callback(question)
  def inform(message: String, stage: String): IO[Unit] = onInform(message, stage)

// ---------------------------------------------------------------------------
// QueueInterviewer -- reads from a pre-filled queue
// ---------------------------------------------------------------------------

/** An interviewer backed by a pre-filled queue of answers.
  * Useful for testing and replay scenarios.
  * Falls back to a default answer when the queue is empty.
  */
class QueueInterviewer private (
    queue: Queue[IO, Answer],
    fallback: Answer
) extends Interviewer:

  def ask(question: Question): IO[Answer] =
    queue.tryTake.map(_.getOrElse(fallback))

  def inform(message: String, stage: String): IO[Unit] = IO.unit

object QueueInterviewer:
  def apply(answers: List[Answer], fallback: Answer = Answer.yes): IO[QueueInterviewer] =
    for
      q <- Queue.unbounded[IO, Answer]
      _ <- answers.traverse_(q.offer)
    yield new QueueInterviewer(q, fallback)

// ---------------------------------------------------------------------------
// RecordingInterviewer -- wraps another and records Q&A pairs
// ---------------------------------------------------------------------------

/** Wraps a delegate interviewer and records all questions and answers
  * for later inspection. Thread-safe via cats-effect Ref.
  */
class RecordingInterviewer private (
    delegate: Interviewer,
    recordRef: cats.effect.Ref[IO, List[(Question, Answer)]]
) extends Interviewer:

  def ask(question: Question): IO[Answer] =
    for
      answer <- delegate.ask(question)
      _      <- recordRef.update(_ :+ (question, answer))
    yield answer

  def inform(message: String, stage: String): IO[Unit] =
    delegate.inform(message, stage)

  def recorded: IO[List[(Question, Answer)]] = recordRef.get

object RecordingInterviewer:
  def apply(delegate: Interviewer): IO[RecordingInterviewer] =
    cats.effect.Ref.of[IO, List[(Question, Answer)]](Nil).map: ref =>
      new RecordingInterviewer(delegate, ref)
