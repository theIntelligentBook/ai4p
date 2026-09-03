package ai4p.neuralnets.widgets

import com.wbillingsley.veautiful.html.{<, SVG, Styling, DHtmlComponent, ^}
import org.scalajs.dom

import ai4p.{*, given}

/**
 * The universal approximation theorem, built up visually: two opposed steep sigmoids ("step
 * activations") subtract to make a single rectangular "pulse", and a single hidden layer of `k`
 * such pulses - each sized and positioned exactly like one of the [[RiemannWidget]]'s rectangles -
 * sums together into an approximation of the target function. Growing the slider grows the hidden
 * layer of the network diagram underneath, so width really is the thing doing the approximating.
 */
object PulseNetworkWidget {
  import ApproxTarget._

  val frame = PlotFrame(xMin = xMin - 0.4, xMax = xMax + 0.4, yMin = yMin, yMax = yMax, w = 560, h = 260)
  val maxPulses = 12
  val steepness = 12.0

  val styling = Styling(
    """|display: inline-block;
       |font-family: 'Lato', sans-serif;
       |""".stripMargin
  ).modifiedBy(
    " .pw-controls" -> "margin-top: 8px; display: flex; gap: 8px; align-items: center; flex-wrap: wrap;",
    " .pw-info" -> "font-family: monospace; font-size: 0.85rem; color: #444; margin-top: 6px;",
    " .pw-label" -> "font-size: 0.85rem; color: #666;",
    " .pw-row" -> "display: flex; gap: 24px; flex-wrap: wrap; align-items: flex-start; margin-top: 10px;",
    " input[type=range]" -> "width: 160px;"
  ).register()

  def sigmoid(x: Double): Double = 1.0 / (1.0 + math.exp(-x))

  case class Pulse(x0: Double, x1: Double, height: Double) {
    def apply(x: Double): Double = sigmoid(steepness * (x - x0)) - sigmoid(steepness * (x - x1))
  }

  /** `k` equal-width pulses spanning [xMin, xMax], each as tall as `f` at its midpoint - same
    * partition as [[RiemannWidget.rectangles]], so the two widgets tell a continuous story. */
  def pulses(k: Int): Vector[Pulse] =
    val width = (xMax - xMin) / k
    Vector.tabulate(k) { i =>
      val x0 = xMin + i * width
      Pulse(x0, x0 + width, f(x0 + width / 2))
    }

  def approx(k: Int)(x: Double): Double = pulses(k).map(p => p.height * p(x)).sum

  /** Network diagram: 1 input -> 2k hidden step-neurons (an on/off pair per pulse) -> 1 linear output. */
  def diagram(k: Int): NetworkView.Diagram =
    val ps = pulses(k)
    val hidden = ps.size * 2
    NetworkView.denseDiagram(
      layerSizes = Vector(1, hidden, 1),
      weight = (li, from, to) =>
        if li == 0 then 1.0
        else
          val pulseIdx = to / 2
          val isOnStep = to % 2 == 0
          val h = ps(pulseIdx).height / yMax
          if isOnStep then h else -h,
      layerLabels = Vector("x", s"$hidden step units", "≈ f(x)"),
      width = math.min(760, 220 + hidden * 22),
      height = 220,
      nodeRadius = if hidden > 16 then 7 else 10
    )
}

case class PulseNetworkWidget() extends DHtmlComponent {
  import PulseNetworkWidget._
  import ApproxTarget.f

  var k: Int = 2
  var showIndividual: Boolean = false

  private def setK(v: Int): Unit =
    k = math.max(1, math.min(maxPulses, v))
    rerender()

  override protected def render =
    val ps = pulses(k)
    <.div(^.cls := styling.className,

      frame.svg(
        frame.axes,
        if showIndividual then
          SVG.g(for p <- ps yield frame.curve(x => p.height * p(x), stroke = "#93c5fd", strokeWidth = 1.5))
        else SVG.g(),
        frame.curve(f, stroke = "#aaa", strokeWidth = 1.5, dash = Some("5 4")),
        frame.curve(approx(k), stroke = "#1d4ed8", strokeWidth = 2.5)
      ),

      <.div(^.cls := "pw-info", s"hidden layer width = ${k * 2} step units (${k} pulses), output = weighted sum of pulses"),

      <.div(^.cls := "pw-controls",
        <.span(^.cls := "pw-label", "pulses ="),
        <("input")(
          ^.attr("type") := "range", ^.attr("min") := "1", ^.attr("max") := maxPulses.toString, ^.attr("step") := "1",
          ^.attr("value") := k.toString,
          ^.on("input") ==> { (e: dom.Event) => setK(e.target.asInstanceOf[dom.html.Input].value.toInt) }
        ),
        <.span(^.cls := "pw-label", k.toString),
        <.label(^.cls := "pw-label",
          <("input")(^.attr("type") := "checkbox",
            ^.on("change") ==> { (_: dom.Event) => showIndividual = !showIndividual; rerender() }),
          " show individual pulses"
        )
      ),

      <.div(^.cls := "pw-row", NetworkView.render(diagram(k)))
    )
}
