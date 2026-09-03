package ai4p.neuralnets.widgets

import com.wbillingsley.veautiful.html.{<, Styling, DHtmlComponent, ^}
import org.scalajs.dom

import ai4p.{*, given}

/**
 * "Building things up in bits": approximates a square wave by summing an increasing number of
 * sine harmonics, so you can watch a rough, wobbly approximation sharpen into the target shape
 * as more frequency terms are added.
 */
object HarmonicsWidget {

  val frame = PlotFrame(xMin = -math.Pi, xMax = 3 * math.Pi, yMin = -1.6, yMax = 1.6, w = 560, h = 300)
  val maxTerms = 20

  val styling = Styling(
    """|display: inline-block;
       |font-family: 'Lato', sans-serif;
       |""".stripMargin
  ).modifiedBy(
    " .hw-controls" -> "margin-top: 8px; display: flex; gap: 8px; align-items: center; flex-wrap: wrap;",
    " .hw-info" -> "font-family: monospace; font-size: 0.9rem; color: #444; margin-top: 6px;",
    " .hw-label" -> "font-size: 0.85rem; color: #666;",
    " input[type=range]" -> "width: 160px;"
  ).register()

  /** The ideal (period 2π) square wave: +1 on (0, π), -1 on (π, 2π). */
  def target(x: Double): Double =
    val m = ((x % (2 * math.Pi)) + 2 * math.Pi) % (2 * math.Pi)
    if m < math.Pi then 1.0 else -1.0

  /** Partial Fourier sum using the first `nTerms` odd harmonics: (4/π) Σ sin((2k-1)x)/(2k-1). */
  def approx(nTerms: Int)(x: Double): Double =
    (1 to nTerms).map { k =>
      val n = 2 * k - 1
      math.sin(n * x) / n
    }.sum * (4.0 / math.Pi)
}

case class HarmonicsWidget() extends DHtmlComponent {
  import HarmonicsWidget._

  var nTerms: Int = 1

  private def setTerms(n: Int): Unit =
    nTerms = math.max(1, math.min(maxTerms, n))
    rerender()

  override protected def render =
    <.div(^.cls := styling.className,
      frame.svg(
        frame.axes,
        frame.curve(target, stroke = "#aaa", strokeWidth = 1.5, dash = Some("5 4")),
        frame.curve(approx(nTerms), stroke = "#3b82f6", strokeWidth = 2.5)
      ),

      <.div(^.cls := "hw-info",
        "f(x) ≈ (4/π) · [ " +
          (1 to nTerms).map(k => s"sin(${2 * k - 1}x)/${2 * k - 1}").mkString(" + ") +
          " ]"
      ),

      <.div(^.cls := "hw-controls",
        <.span(^.cls := "hw-label", "harmonics ="),
        <("input")(
          ^.attr("type") := "range", ^.attr("min") := "1", ^.attr("max") := maxTerms.toString, ^.attr("step") := "1",
          ^.attr("value") := nTerms.toString,
          ^.on("input") ==> { (e: dom.Event) => setTerms(e.target.asInstanceOf[dom.html.Input].value.toInt) }
        ),
        <.span(^.cls := "hw-label", nTerms.toString),
        <.button(^.cls := "btn btn-outline-secondary btn-sm", ^.onClick --> setTerms(1), "Reset")
      )
    )
}
