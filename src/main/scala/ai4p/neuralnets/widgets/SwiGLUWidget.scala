package ai4p.neuralnets.widgets

import com.wbillingsley.veautiful.html.{<, Styling, DHtmlComponent, ^}
import org.scalajs.dom

import ai4p.{*, given}

/**
 * The real SwiGLU: two independent linear projections of the same input - a "gate" (passed
 * through SiLU) and a plain "value" - multiplied together elementwise. Where the previous slide
 * showed SiLU on its own, this is the actual gated unit used in modern transformer feedforward
 * layers (PaLM, LLaMA, ...):
 *
 *     SwiGLU(x) = SiLU(w_gate · x) * (w_value · x)
 *
 * Two independent weights, two independent sliders - watch the gate's SiLU shape get reweighted
 * (and, past zero, sign-flipped) by the otherwise perfectly linear value branch.
 */
object SwiGLUWidget {

  val xMin = -4.0
  val xMax = 4.0
  val yMin = -4.0
  val yMax = 16.0
  val wMin = -2.0
  val wMax = 2.0

  val frame = PlotFrame(xMin = xMin, xMax = xMax, yMin = yMin, yMax = yMax, w = 480, h = 320)

  def sigmoid(z: Double): Double = 1.0 / (1.0 + math.exp(-z))
  def silu(z: Double): Double = z * sigmoid(z)

  val styling = Styling(
    """|display: inline-block;
       |font-family: 'Lato', sans-serif;
       |""".stripMargin
  ).modifiedBy(
    " .sg-controls" -> "margin-top: 8px; display: flex; gap: 8px; align-items: center; flex-wrap: wrap;",
    " .sg-info" -> "font-family: monospace; font-size: 0.85rem; color: #444; margin-top: 6px;",
    " .sg-label" -> "font-size: 0.85rem; color: #666;",
    " .sg-row" -> "display: flex; gap: 24px; flex-wrap: wrap; align-items: center;",
    " input[type=range]" -> "width: 140px;"
  ).register()

  def diagram(wGate: Double, wValue: Double): NetworkView.Diagram =
    NetworkView.denseDiagram(
      layerSizes = Vector(1, 2, 1),
      weight = (li, from, to) =>
        if li == 0 && to == 0 then wGate / wMax
        else if li == 0 && to == 1 then wValue / wMax
        else 1.0,
      label = (li, ni) =>
        if li == 0 then "x"
        else if li == 1 && ni == 0 then "gate"
        else if li == 1 then "value"
        else "×",
      layerLabels = Vector("x", "SiLU / linear", "y"),
      width = 260, height = 200, nodeRadius = 16
    )
}

case class SwiGLUWidget() extends DHtmlComponent {
  import SwiGLUWidget._

  private var wGate: Double = 1.0
  private var wValue: Double = 1.0

  private def setGate(v: Double): Unit =
    wGate = math.max(wMin, math.min(wMax, v))
    rerender()

  private def setValue(v: Double): Unit =
    wValue = math.max(wMin, math.min(wMax, v))
    rerender()

  private def reset(): Unit =
    wGate = 1.0; wValue = 1.0
    rerender()

  private def output(x: Double): Double = silu(wGate * x) * (wValue * x)

  override protected def render =
    <.div(^.cls := styling.className,
      <.div(^.cls := "sg-row",
        NetworkView.render(diagram(wGate, wValue)),
        frame.svg(
          frame.axes,
          frame.curve(output, stroke = "#7c3aed", strokeWidth = 2.5)
        )
      ),

      <.div(^.cls := "sg-info",
        f"y = SiLU(w_gate·x) × (w_value·x),   w_gate = $wGate%.1f,   w_value = $wValue%.1f"),

      <.div(^.cls := "sg-controls",
        <.span(^.cls := "sg-label", "w_gate ="),
        <("input")(
          ^.attr("type") := "range", ^.attr("min") := wMin.toString, ^.attr("max") := wMax.toString, ^.attr("step") := "0.1",
          ^.attr("value") := wGate.toString,
          ^.on("input") ==> { (e: dom.Event) => setGate(e.target.asInstanceOf[dom.html.Input].value.toDouble) }
        ),
        <.span(^.cls := "sg-label", f"$wGate%.1f"),

        <.span(^.cls := "sg-label", "w_value ="),
        <("input")(
          ^.attr("type") := "range", ^.attr("min") := wMin.toString, ^.attr("max") := wMax.toString, ^.attr("step") := "0.1",
          ^.attr("value") := wValue.toString,
          ^.on("input") ==> { (e: dom.Event) => setValue(e.target.asInstanceOf[dom.html.Input].value.toDouble) }
        ),
        <.span(^.cls := "sg-label", f"$wValue%.1f"),

        <.button(^.cls := "btn btn-outline-secondary btn-sm", ^.onClick --> reset(), "Reset")
      )
    )
}
