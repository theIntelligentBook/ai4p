package ai4p.uncertainty.widgets

import com.wbillingsley.veautiful.html.{<, Styling, DHtmlComponent, VHtmlContent, EventMethods, ^}
import scala.util.Random
import scala.collection.mutable

import ai4p.{*, given}
import Common.*

import site.given

/**
 * A word-level Markov model whose entire vocabulary and transition table come from a single
 * source text — by default, A. A. Milne's poem ''Disobedience''. Each line ends with an
 * end-of-line token, so the model also learns (crudely) where lines tend to break.
 *
 * The order is configurable: with `order = 1` (bigram) the next word depends only on the current
 * word; with `order = 2` (trigram) it depends on the last two tokens (which may include the
 * end-of-line token itself, so it can learn what tends to start a line).
 */
object TextMarkov {

  /** End-of-line token. Also used to pad the start of the text, so the very first word of the
   *  source is modelled as "the word that follows a line break". */
  val EOL = "⏎" // ⏎

  val poemText: String =
    """James James
      |Morrison Morrison
      |Weatherby George Dupree
      |Took great
      |Care of his Mother,
      |Though he was only three.
      |James James Said to his Mother,
      |"Mother," he said, said he;
      |"You must never go down
      |to the end of the town,
      |if you don't go down with me."
      |James James
      |Morrison's Mother
      |Put on a golden gown.
      |James James Morrison's Mother
      |Drove to the end of the town.
      |James James Morrison's Mother
      |Said to herself, said she:
      |"I can get right down
      |to the end of the town
      |and be back in time for tea."
      |King John
      |Put up a notice,
      |"LOST or STOLEN or STRAYED!
      |JAMES JAMES MORRISON'S MOTHER
      |SEEMS TO HAVE BEEN MISLAID.
      |LAST SEEN
      |WANDERING VAGUELY:
      |QUITE OF HER OWN ACCORD,
      |SHE TRIED TO GET DOWN
      |TO THE END OF THE TOWN -
      |FORTY SHILLINGS REWARD!"
      |James James
      |Morrison Morrison
      |(Commonly known as Jim)
      |Told his
      |Other relations
      |Not to go blaming him.
      |James James
      |Said to his Mother,
      |"Mother," he said, said he:
      |"You must never go down to the end of the town
      |without consulting me."
      |James James
      |Morrison's mother
      |Hasn't been heard of since.
      |King John said he was sorry,
      |So did the Queen and Prince.
      |King John
      |(Somebody told me)
      |Said to a man he knew:
      |If people go down to the end of the town, well,
      |what can anyone do?"
      |""".stripMargin

  /** Lower-cases and strips punctuation from a raw whitespace-delimited chunk, keeping internal
   *  apostrophes (so "Morrison's" and "don't" survive as single words). */
  def normalizeWord(raw: String): String =
    raw.toLowerCase.filter(c => c.isLetterOrDigit || c == '\'').stripPrefix("'").stripSuffix("'")

  def tokenizeLine(line: String): Vector[String] =
    line.split("\\s+").toVector.map(normalizeWord).filter(_.nonEmpty)

  /** Turns free text into a flat token stream, with an [[EOL]] token inserted after every
   *  (non-blank) line — this is where the vocabulary and "grammar" of the model come from. */
  def tokenize(text: String): Vector[String] =
    text.linesIterator.map(tokenizeLine).filter(_.nonEmpty).flatMap(_ :+ EOL).toVector

  /** context (the last `order` tokens) -> distribution over what came next, purely from counts in `tokens`. */
  def buildModel(tokens: Vector[String], order: Int): Map[Vector[String], Vector[(String, Double)]] =
    val counts = mutable.LinkedHashMap.empty[Vector[String], mutable.LinkedHashMap[String, Int]]
    val padded = Vector.fill(order)(EOL) ++ tokens
    for i <- order until padded.size do
      val context = padded.slice(i - order, i)
      val next = padded(i)
      val forContext = counts.getOrElseUpdate(context, mutable.LinkedHashMap.empty)
      forContext(next) = forContext.getOrElse(next, 0) + 1
    counts.view.mapValues { forContext =>
      val total = forContext.values.sum.toDouble
      forContext.toVector.map((word, c) => (word, c / total)).sortBy(-_._2)
    }.toMap

  def sample(dist: Vector[(String, Double)], rng: Random): String =
    val r = rng.nextDouble()
    var cum = 0.0
    var i = 0
    while i < dist.size - 1 && { cum += dist(i)._2; r > cum } do i += 1
    dist(i)._1

  def generate(model: Map[Vector[String], Vector[(String, Double)]], order: Int, length: Int, rng: Random = new Random()): Vector[String] =
    val out = mutable.ArrayBuffer.empty[String]
    var context = Vector.fill(order)(EOL)
    for _ <- 0 until length do
      val dist = model.getOrElse(context, Vector(EOL -> 1.0))
      val next = sample(dist, rng)
      out += next
      context = (context :+ next).takeRight(order)
    out.toVector

  /** Turns a token stream back into readable text, one line per [[EOL]] — with the [[EOL]] token
   *  itself shown (as ⏎) at the end of the line it terminates, so it's visible which of the
   *  model's choices were "end this line" rather than another word. */
  def toText(tokens: Vector[String]): String =
    val sb = new StringBuilder
    for tok <- tokens do
      val display = if tok == EOL then EOL else tok
      if sb.nonEmpty && sb.last != '\n' then sb.append(' ')
      sb.append(display)
      if tok == EOL then sb.append('\n')
    sb.toString

  val styling = Styling(
    """|display: inline-block;
       |font-family: 'Lato', sans-serif;
       |max-width: 900px;
       |""".stripMargin
  ).modifiedBy(
    " .tmw-columns" -> "display: flex; gap: 16px; flex-wrap: wrap; margin: 10px 0;",
    " .tmw-col" -> "flex: 1 1 320px; min-width: 280px;",
    " .tmw-col-title" -> "font-size: 0.85rem; font-weight: bold; color: #555; margin-bottom: 4px;",
    " .tmw-textarea" -> "width: 100%; height: 260px; box-sizing: border-box; font-family: monospace; font-size: 0.85rem; padding: 8px; border: 1px solid #ccc; border-radius: 4px; resize: vertical;",
    " .tmw-controls" -> "margin-top: 10px; display: flex; gap: 8px; align-items: center; flex-wrap: wrap;"
  ).register()
}

case class TextMarkovWidget(outputLength: Int = 80) extends DHtmlComponent {
  import TextMarkov._

  val rng: Random = new Random()

  var sourceText: String = poemText
  var order: Int = 2 // trigram by default: last two tokens

  var model: Map[Vector[String], Vector[(String, Double)]] = buildModel(tokenize(sourceText), order)
  var generatedText: String = toText(generate(model, order, outputLength, rng))

  def updateSource(v: String): Unit =
    sourceText = v

  def rebuildAndGenerate(): Unit =
    model = buildModel(tokenize(sourceText), order)
    generatedText = toText(generate(model, order, outputLength, rng))
    rerender()

  def setOrder(o: Int): Unit =
    if o != order then
      order = o
      rebuildAndGenerate()

  override protected def render =
    <.div(^.cls := TextMarkov.styling.className,
      <.p(^.cls := "br-label",
        "Paste your own text on the left (or keep Disobedience), choose bigram or trigram, and " +
        "hit the button to rebuild the model from that text and generate a new sequence on the " +
        "right. ⏎ marks an end-of-line token."),

      <.div(^.cls := "tmw-columns",
        <.div(^.cls := "tmw-col",
          <.div(^.cls := "tmw-col-title", "Source text (vocabulary + transition counts)"),
          <.textarea(^.cls := "tmw-textarea",
            sourceText, // sets the initial value on first mount
            ^.prop.value := sourceText, // keeps it in sync on later re-renders
            ^.onInput ==> (e => for v <- e.inputValue do updateSource(v))
          )
        ),
        <.div(^.cls := "tmw-col",
          <.div(^.cls := "tmw-col-title", "Generated text"),
          <.textarea(^.cls := "tmw-textarea",
            generatedText, // sets the initial value on first mount
            ^.prop.value := generatedText, // keeps it in sync on later re-renders
            ^.attr.readonly := "readonly"
          )
        )
      ),

      <.div(^.cls := "tmw-controls",
        <.button(^.cls := s"btn btn-sm ${if order == 1 then "btn-primary" else "btn-outline-primary"}",
          ^.onClick --> setOrder(1), "Bigram (last 1 word)"),
        <.button(^.cls := s"btn btn-sm ${if order == 2 then "btn-primary" else "btn-outline-primary"}",
          ^.onClick --> setOrder(2), "Trigram (last 2 words)"),
        <.button(^.cls := "btn btn-primary btn-sm", ^.onClick --> rebuildAndGenerate(), "Rebuild model & generate")
      )
    )
}
