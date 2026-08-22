package ai4p.uncertainty.widgets

import com.wbillingsley.veautiful.html.{<, Styling, DHtmlComponent, VHtmlContent, ^}
import scala.util.Random
import scala.collection.mutable.ArrayBuffer

import ai4p.{*, given}
import Common.*

import site.given

/**
 * The very first Markov chain. In 1913, Andrei Markov analysed the first 20,000 letters of
 * Pushkin's poem ''Eugene Onegin'', classifying each one as a vowel or a consonant, and counted
 * how often a vowel was followed by another vowel versus a consonant (and the same starting from
 * a consonant). The transition probabilities below are the commonly-cited approximations of what
 * he found — vowels and consonants alternate far more than chance alone would produce, which is
 * exactly the kind of structure a two-state Markov chain is enough to capture.
 *
 * It's also about as simple as a Markov model can get: two states, a 2x2 transition table, done.
 */
object VowelConsonant {

  enum Kind:
    case Vowel, Consonant

  import Kind._

  /** Markov's approximate original transition probabilities (Eugene Onegin, 1913). */
  val transition: Map[Kind, Vector[(Kind, Double)]] = Map(
    Vowel -> Vector(Vowel -> 0.128, Consonant -> 0.872),
    Consonant -> Vector(Vowel -> 0.663, Consonant -> 0.337)
  )

  def sampleNext(current: Kind, rng: Random): Kind =
    val pVowel = transition(current).head._2
    if rng.nextDouble() < pVowel then Vowel else Consonant

  def generate(length: Int, rng: Random = new Random()): Vector[Kind] =
    val out = ArrayBuffer.empty[Kind]
    var state = if rng.nextBoolean() then Vowel else Consonant
    out += state
    for _ <- 1 until length do
      state = sampleNext(state, rng)
      out += state
    out.toVector

  val styling = Styling(
    """|display: inline-block;
       |font-family: 'Lato', sans-serif;
       |max-width: 820px;
       |""".stripMargin
  ).modifiedBy(
    " .vc-sequence" -> "display: flex; gap: 3px; flex-wrap: wrap; margin: 10px 0;",
    " .vc-letter" -> "display: inline-flex; align-items: center; justify-content: center; width: 22px; height: 22px; border-radius: 3px; font-weight: bold; font-size: 0.85rem; cursor: pointer; border: 1px solid transparent;",
    " .vc-letter.vc-vowel" -> "background: #fee2e2; color: #991b1b;",
    " .vc-letter.vc-consonant" -> "background: #dbeafe; color: #1e40af;",
    " .vc-letter.vc-selected" -> "border-color: #333;",
    " .vc-section-title" -> "font-size: 0.85rem; font-weight: bold; color: #555; margin-top: 8px;",
    " .vc-bar-row" -> "display: flex; align-items: center; gap: 6px; margin: 2px 0; font-size: 0.85rem;",
    " .vc-bar-row.vc-actual" -> "font-weight: bold; color: #166534;",
    " .vc-bar-label" -> "width: 74px; color: #666;",
    " .vc-bar-track" -> "flex: 1; height: 10px; background: #eee; border-radius: 3px; overflow: hidden; max-width: 240px;",
    " .vc-bar-fill" -> "height: 100%; background: #a855f7;",
    " .vc-bar-row.vc-actual .vc-bar-fill" -> "background: #16a34a;",
    " .vc-bar-pct" -> "width: 34px; font-size: 0.75rem; color: #666;",
    " .vc-controls" -> "margin-top: 10px; display: flex; gap: 8px; align-items: center; flex-wrap: wrap;"
  ).register()
}

case class VowelConsonantWidget(length: Int = 30) extends DHtmlComponent {
  import VowelConsonant._
  import Kind._

  var sequence: Vector[Kind] = generate(length)
  var selected: Int = 0

  def regenerate(): Unit =
    sequence = generate(length)
    selected = 0
    rerender()

  def select(i: Int): Unit =
    selected = i
    rerender()

  override protected def render =
    val current = sequence(selected)
    val dist = transition(current)
    val actualNext = if selected < sequence.size - 1 then Some(sequence(selected + 1)) else None

    <.div(^.cls := styling.className,
      <.p(^.cls := "br-label", "A sequence of V(owel)s and C(onsonant)s generated from just a 2x2 transition table. Click a letter to see the probabilities that decided what came next."),

      <.div(^.cls := "vc-sequence",
        for (k, i) <- sequence.zipWithIndex yield
          <.span(
            ^.cls := s"vc-letter ${if k == Vowel then "vc-vowel" else "vc-consonant"}${if i == selected then " vc-selected" else ""}",
            ^.onClick --> select(i),
            if k == Vowel then "V" else "C"
          )
      ),

      <.div(^.cls := "vc-section-title", s"P(next | current = $current)"),
      <.div(
        for (k, p) <- dist yield
          <.div(^.cls := s"vc-bar-row${if actualNext.contains(k) then " vc-actual" else ""}",
            <.span(^.cls := "vc-bar-label", k.toString),
            <.div(^.cls := "vc-bar-track", <.div(^.cls := "vc-bar-fill", ^.style := s"width: ${(p * 100).round}%;")),
            <.span(^.cls := "vc-bar-pct", s"${(p * 100).round}%")
          )
      ),

      <.div(^.cls := "vc-controls",
        <.button(^.cls := "btn btn-outline-primary btn-sm", ^.onClick --> regenerate(), "Generate new sequence")
      )
    )
}
