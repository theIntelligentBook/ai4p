package ai4p.uncertainty.widgets

import com.wbillingsley.veautiful.html.{<, Styling, DHtmlComponent, VHtmlContent, ^}
import org.scalajs.dom
import scala.util.Random

import ai4p.{*, given}
import Common.*

import site.given

object MorseHmmWidget {
  val styling = Styling(
    """|display: inline-block;
       |font-family: 'Lato', sans-serif;
       |max-width: 820px;
       |""".stripMargin
  ).modifiedBy(
    " .mkh-row" -> "display: flex; align-items: center; gap: 4px; margin: 4px 0;",
    " .mkh-row-label" -> "width: 54px; font-size: 0.75rem; color: #666; text-transform: uppercase;",
    " .mkh-cell" -> "display: inline-flex; align-items: center; justify-content: center; width: 26px; height: 26px; font-weight: bold; font-family: monospace; border-radius: 3px;",
    " .mkh-cell.mkh-wrong" -> "background: #fee2e2; color: #991b1b;",
    " .mkh-code-cell" -> "display: inline-flex; width: 26px; justify-content: center; gap: 1px; font-family: monospace; font-size: 0.85rem; color: #444;",
    " .mkh-symbol.mkh-flipped" -> "color: #ea580c; font-weight: bold;",
    " .mkh-score" -> "font-size: 0.9rem; margin: 8px 0; color: #333;",
    " .mkh-controls" -> "margin-top: 8px; display: flex; gap: 8px; align-items: center; flex-wrap: wrap;",
    " input[type=range]" -> "width: 120px;"
  ).register()
}

/**
 * A short message sent over a noisy channel: each of the (toy, fixed-length) Morse-style symbols
 * gets independently flipped with probability `noiseP`. Compares two ways of decoding it back:
 * deciding each letter alone from its noisy symbols (ignoring everything around it), versus
 * Viterbi decoding, which uses the Markov chain's letter-transition probabilities as context to
 * prefer plausible letter *sequences* — exactly the "hidden" in Hidden Markov Model: the true
 * letters are never observed directly, only a noisy signal generated from them.
 */
case class MorseHmmWidget(length: Int = 8, initialNoise: Double = 0.15) extends DHtmlComponent {
  import MorseModel._

  var trueSeq: Vector[Int] = generate(length)
  var noiseP: Double = initialNoise
  var observed: Vector[Code] = reNoised()

  def reNoised(): Vector[Code] = trueSeq.map(i => applyNoise(letters(i).code, noiseP, new Random()))

  def renoise(): Unit =
    observed = reNoised()
    rerender()

  def newMessage(): Unit =
    trueSeq = generate(length)
    observed = reNoised()
    rerender()

  def setNoise(p: Double): Unit =
    noiseP = p
    observed = reNoised()
    rerender()

  private def letterRow(label: String, seq: Vector[Int], markWrong: Boolean): VHtmlContent =
    <.div(^.cls := "mkh-row",
      <.span(^.cls := "mkh-row-label", label),
      for (idx, i) <- seq.zipWithIndex yield
        <.span(^.cls := s"mkh-cell${if markWrong && idx != trueSeq(i) then " mkh-wrong" else ""}", letters(idx).char.toString)
    )

  private def codeRow(): VHtmlContent =
    <.div(^.cls := "mkh-row",
      <.span(^.cls := "mkh-row-label", "signal"),
      for (obs, i) <- observed.zipWithIndex yield
        val trueCode = letters(trueSeq(i)).code
        <.span(^.cls := "mkh-code-cell",
          for (bit, j) <- obs.zipWithIndex yield
            <.span(^.cls := s"mkh-symbol${if bit != trueCode(j) then " mkh-flipped" else ""}", if bit then "-" else ".")
        )
    )

  override protected def render =
    val naive = naiveDecode(observed, noiseP)
    val viterbi = viterbiDecode(observed, noiseP)
    val naiveCorrect = naive.zip(trueSeq).count(_ == _)
    val viterbiCorrect = viterbi.zip(trueSeq).count(_ == _)

    <.div(^.cls := MorseHmmWidget.styling.className,
      <.p(^.cls := "br-label", "A message sent over a noisy channel — some dots/dashes get flipped in transit (shown in orange). Compare deciding each letter alone against letting the Markov chain's context help."),

      letterRow("true", trueSeq, markWrong = false),
      codeRow(),
      letterRow("naive", naive, markWrong = true),
      letterRow("HMM", viterbi, markWrong = true),

      <.p(^.cls := "mkh-score", s"Naive (no context): $naiveCorrect/$length correct.   HMM (Viterbi, uses context): $viterbiCorrect/$length correct."),

      <.div(^.cls := "mkh-controls",
        <.span(^.cls := "br-label", "noise σ ="),
        <("input")(
          ^.attr("type") := "range", ^.attr("min") := "0.0", ^.attr("max") := "0.45", ^.attr("step") := "0.05",
          ^.attr("value") := noiseP.toString,
          ^.on("input") ==> { (e: dom.Event) =>
            setNoise(e.target.asInstanceOf[dom.html.Input].value.toDouble)
          }
        ),
        <.span(^.cls := "br-label", f"$noiseP%.2f"),
        <.button(^.cls := "btn btn-outline-primary btn-sm", ^.onClick --> renoise(), "Re-noise (same message)"),
        <.button(^.cls := "btn btn-outline-secondary btn-sm", ^.onClick --> newMessage(), "New message")
      )
    )
}
