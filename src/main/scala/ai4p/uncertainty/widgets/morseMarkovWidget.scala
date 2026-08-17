package ai4p.uncertainty.widgets

import com.wbillingsley.veautiful.html.{<, Styling, DHtmlComponent, VHtmlContent, ^}
import scala.util.Random

import ai4p.{*, given}
import Common.*

import site.given

object MorseMarkovWidget {
  val styling = Styling(
    """|display: inline-block;
       |font-family: 'Lato', sans-serif;
       |max-width: 820px;
       |""".stripMargin
  ).modifiedBy(
    " .mkw-sequence" -> "display: flex; gap: 6px; flex-wrap: wrap; margin: 10px 0;",
    " .mkw-letter" -> "display: flex; flex-direction: column; align-items: center; padding: 6px 8px; border: 1px solid #ccc; border-radius: 4px; background: white; cursor: pointer; min-width: 34px;",
    " .mkw-letter.mkw-selected" -> "border-color: #3b82f6; background: #eff6ff;",
    " .mkw-char" -> "font-weight: bold; font-size: 1.1rem;",
    " .mkw-code" -> "font-family: monospace; font-size: 0.85rem; color: #666;",
    " .mkw-section-title" -> "font-size: 0.85rem; font-weight: bold; color: #555; margin-top: 8px;",
    " .mkw-bar-row" -> "display: flex; align-items: center; gap: 6px; margin: 2px 0; font-size: 0.85rem;",
    " .mkw-bar-row.mkw-actual" -> "font-weight: bold; color: #166534;",
    " .mkw-bar-label" -> "width: 16px; text-align: right; color: #666;",
    " .mkw-bar-track" -> "flex: 1; height: 10px; background: #eee; border-radius: 3px; overflow: hidden; max-width: 240px;",
    " .mkw-bar-fill" -> "height: 100%; background: #3b82f6;",
    " .mkw-bar-row.mkw-actual .mkw-bar-fill" -> "background: #16a34a;",
    " .mkw-bar-pct" -> "width: 34px; font-size: 0.75rem; color: #666;",
    " .mkw-controls" -> "margin-top: 10px; display: flex; gap: 8px; align-items: center; flex-wrap: wrap;"
  ).register()
}

/**
 * A first-order Markov chain generating letters (from [[MorseModel]]'s small toy alphabet), shown
 * alongside their (toy, fixed-length) Morse-style codes. Click any letter in the generated
 * sequence to see the transition probabilities that actually produced whatever comes after it —
 * the *only* thing a first-order Markov model needs to predict the next symbol is the current one.
 */
case class MorseMarkovWidget(length: Int = 10) extends DHtmlComponent {
  import MorseModel._

  var sequence: Vector[Int] = generate(length)
  var selected: Int = 0

  def regenerate(): Unit =
    sequence = generate(length)
    selected = 0
    rerender()

  def select(i: Int): Unit =
    selected = i
    rerender()

  private def barChart(dist: Vector[Double], actual: Option[Int]): VHtmlContent =
    <.div(
      for (p, i) <- dist.zipWithIndex yield
        <.div(^.cls := s"mkw-bar-row${if actual.contains(i) then " mkw-actual" else ""}",
          <.span(^.cls := "mkw-bar-label", letters(i).char.toString),
          <.div(^.cls := "mkw-bar-track", <.div(^.cls := "mkw-bar-fill", ^.style := s"width: ${(p * 100).round}%;")),
          <.span(^.cls := "mkw-bar-pct", s"${(p * 100).round}%")
        )
    )

  override protected def render =
    val actualNext = if selected < sequence.size - 1 then Some(sequence(selected + 1)) else None

    <.div(^.cls := MorseMarkovWidget.styling.className,
      <.p(^.cls := "br-label", "A sequence generated letter-by-letter from the Markov chain. Click a letter to see the probabilities that decided what came next."),

      <.div(^.cls := "mkw-sequence",
        for (letterIdx, i) <- sequence.zipWithIndex yield
          <.div(^.cls := s"mkw-letter${if i == selected then " mkw-selected" else ""}", ^.onClick --> select(i),
            <.div(^.cls := "mkw-char", letters(letterIdx).char.toString),
            <.div(^.cls := "mkw-code", codeString(letters(letterIdx).code))
          )
      ),

      {
        if selected < sequence.size - 1 then
          <.div(
            <.div(^.cls := "mkw-section-title", s"P(next letter | current = ${letters(sequence(selected)).char})"),
            barChart(transition(sequence(selected)), actualNext)
          )
        else
          <.p(^.cls := "br-label", "(end of sequence — no next letter to predict)")
      },

      <.div(^.cls := "mkw-controls",
        <.button(^.cls := "btn btn-outline-primary btn-sm", ^.onClick --> regenerate(), "Generate new sequence")
      )
    )
}
