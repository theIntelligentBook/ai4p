package ai4p.neuralnets.widgets

import com.wbillingsley.veautiful.html.{<, SVG, Styling, DHtmlComponent, ^}
import org.scalajs.dom

import ai4p.{*, given}

/**
 * Estimates the area under a curve using a slider-controlled number of rectangles (a Riemann sum,
 * rather than the usual trapezium rule - flat-topped rectangles are the shape we'll shortly build
 * out of a pair of step activations, so this widget sets up that story visually).
 */
object RiemannWidget {
  import ApproxTarget._

  val frame = PlotFrame(xMin = xMin - 0.4, xMax = xMax + 0.4, yMin = yMin, yMax = yMax, w = 560, h = 300)
  val maxRects = 40

  val styling = Styling(
    """|display: inline-block;
       |font-family: 'Lato', sans-serif;
       |""".stripMargin
  ).modifiedBy(
    " .rw-controls" -> "margin-top: 8px; display: flex; gap: 8px; align-items: center; flex-wrap: wrap;",
    " .rw-info" -> "font-family: monospace; font-size: 0.9rem; color: #444; margin-top: 6px;",
    " .rw-label" -> "font-size: 0.85rem; color: #666;",
    " input[type=range]" -> "width: 160px;"
  ).register()

  /** Midpoint-rule rectangles: `n` equal-width bars over [xMin, xMax], each as tall as f at its midpoint. */
  def rectangles(n: Int): Vector[(Double, Double, Double)] =
    val width = (xMax - xMin) / n
    Vector.tabulate(n) { i =>
      val x0 = xMin + i * width
      val mid = x0 + width / 2
      (x0, x0 + width, f(mid))
    }

  def estimate(n: Int): Double =
    val width = (xMax - xMin) / n
    rectangles(n).map(_._3).sum * width
}

case class RiemannWidget() extends DHtmlComponent {
  import RiemannWidget._
  import ApproxTarget.f

  var n: Int = 4
  private val trueArea = ApproxTarget.trueIntegral()

  private def setN(v: Int): Unit =
    n = math.max(1, math.min(maxRects, v))
    rerender()

  override protected def render =
    val est = estimate(n)
    <.div(^.cls := styling.className,
      frame.svg(
        frame.axes,
        SVG.g(
          for (x0, x1, height) <- rectangles(n) yield
            frame.rect(x0, x1, 0, height, fill = "#3b82f6", opacity = 0.3)
        ),
        frame.curve(f, stroke = "#1d4ed8", strokeWidth = 2.5)
      ),

      <.div(^.cls := "rw-info", f"estimated area ≈ $est%.3f   (true area ≈ $trueArea%.3f)"),

      <.div(^.cls := "rw-controls",
        <.span(^.cls := "rw-label", "rectangles ="),
        <("input")(
          ^.attr("type") := "range", ^.attr("min") := "1", ^.attr("max") := maxRects.toString, ^.attr("step") := "1",
          ^.attr("value") := n.toString,
          ^.on("input") ==> { (e: dom.Event) => setN(e.target.asInstanceOf[dom.html.Input].value.toInt) }
        ),
        <.span(^.cls := "rw-label", n.toString)
      )
    )
}
