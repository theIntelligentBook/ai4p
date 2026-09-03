package ai4p.neuralnets.widgets

import com.wbillingsley.veautiful.html.{<, Styling, DHtmlComponent, ^}
import org.scalajs.dom

import ai4p.{*, given}


/**
 * Approximates sin(x) around x=0 with an increasing number of Taylor-series terms, showing the
 * polynomial hugging the target curve near the centre and then peeling away once you get far
 * enough from it - a different flavour of "building a function up in bits" to the harmonics widget.
 */
object TaylorSeriesWidget {

  val frame = PlotFrame(xMin = -3 * math.Pi, xMax = 3 * math.Pi, yMin = -2.2, yMax = 2.2, w = 560, h = 300)
  val maxTerms = 12

  val styling = Styling(
    """|display: inline-block;
       |font-family: 'Lato', sans-serif;
       |""".stripMargin
  ).modifiedBy(
    " .ts-controls" -> "margin-top: 8px; display: flex; gap: 8px; align-items: center; flex-wrap: wrap;",
    " .ts-info" -> "font-family: monospace; font-size: 0.85rem; color: #444; margin-top: 6px; word-break: break-word;",
    " .ts-label" -> "font-size: 0.85rem; color: #666;",
    " input[type=range]" -> "width: 160px;"
  ).register()

  def factorial(n: Int): Double = (1 to n).foldLeft(1.0)(_ * _)

  /** Taylor polynomial for sin(x) about 0, using `nTerms` non-zero (odd-power) terms. */
  def approx(nTerms: Int)(x: Double): Double =
    (0 until nTerms).map { k =>
      val n = 2 * k + 1
      val sign = if k % 2 == 0 then 1.0 else -1.0
      sign * math.pow(x, n) / factorial(n)
    }.sum

  def termString(nTerms: Int): String =
    (0 until nTerms).map { k =>
      val n = 2 * k + 1
      val sign = if k % 2 == 0 then "+" else "-"
      s"$sign x^$n/$n!"
    }.mkString(" ").stripPrefix("+ ")
}

case class TaylorSeriesWidget() extends DHtmlComponent {
  import TaylorSeriesWidget._

  var nTerms: Int = 1

  private def setTerms(n: Int): Unit =
    nTerms = math.max(1, math.min(maxTerms, n))
    rerender()

  override protected def render =
    <.div(^.cls := styling.className,
      frame.svg(
        frame.axes,
        frame.curve(math.sin, stroke = "#aaa", strokeWidth = 1.5, dash = Some("5 4")),
        frame.curve(approx(nTerms), stroke = "#16a34a", strokeWidth = 2.5)
      ),

      <.div(^.cls := "ts-info", s"sin(x) ≈ ${termString(nTerms)}"),

      <.div(^.cls := "ts-controls",
        <.span(^.cls := "ts-label", "terms ="),
        <("input")(
          ^.attr("type") := "range", ^.attr("min") := "1", ^.attr("max") := maxTerms.toString, ^.attr("step") := "1",
          ^.attr("value") := nTerms.toString,
          ^.on("input") ==> { (e: dom.Event) => setTerms(e.target.asInstanceOf[dom.html.Input].value.toInt) }
        ),
        <.span(^.cls := "ts-label", nTerms.toString),
        <.button(^.cls := "btn btn-outline-secondary btn-sm", ^.onClick --> setTerms(1), "Reset")
      )
    )
}
