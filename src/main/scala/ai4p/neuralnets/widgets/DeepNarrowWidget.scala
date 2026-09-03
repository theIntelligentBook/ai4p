package ai4p.neuralnets.widgets

import com.wbillingsley.veautiful.html.{<, Styling, DHtmlComponent, ^}
import org.scalajs.dom

import ai4p.{*, given}

/**
 * "Deep networks are more efficient": a network that is only **two neurons wide**, however deep it
 * gets, approximating a plain quadratic - with no training at all. One neuron composes a
 * triangle-wave activation with itself layer after layer; the other accumulates a fixed,
 * exactly-derived weighted sum of those compositions that converges (provably, not just
 * empirically) to `(x-x0)^2`. Growing the depth slider adds one more term of that convergent
 * series, so the fit visibly sharpens towards an exact match. Compare with [[WideDeepWidget]],
 * which gives up the strict width-2 constraint to chase a much wobblier target via real training.
 */
object DeepNarrowWidget {
  import ApproxTarget._
  import DeepNarrowNetModel.coefficient

  val frame = PlotFrame(xMin = xMin - 0.4, xMax = xMax + 0.4, yMin = yMin, yMax = yMax, w = 560, h = 280)

  val styling = Styling(
    """|display: inline-block;
       |font-family: 'Lato', sans-serif;
       |""".stripMargin
  ).modifiedBy(
    " .dn-controls" -> "margin-top: 8px; display: flex; gap: 8px; align-items: center; flex-wrap: wrap;",
    " .dn-info" -> "font-family: monospace; font-size: 0.85rem; color: #444; margin-top: 6px;",
    " .dn-label" -> "font-size: 0.85rem; color: #666;",
    " .dn-row" -> "display: flex; gap: 24px; flex-wrap: wrap; align-items: flex-start; margin-top: 10px;",
    " input[type=range]" -> "width: 160px;"
  ).register()

  def diagram(depth: Int): NetworkView.Diagram =
    val scale = math.max(1.0, (1 to depth).map(j => math.abs(coefficient(j))).max)
    NetworkView.denseDiagram(
      layerSizes = Vector.fill(depth + 1)(2),
      weight = (li, from, to) =>
        if from == 0 && to == 0 then 1.0
        else if from == 0 && to == 1 then coefficient(li + 1) / scale
        else if from == 1 && to == 0 then 0.0
        else 1.0,
      label = (li, ni) =>
        if li == 0 && ni == 0 then "x"
        else if li == 0 then "base"
        else if ni == 0 then "tent"
        else "Σ",
      layerLabels = Vector("x") ++ Vector.fill(depth - 1)("") ++ Vector("≈ quadratic fit"),
      width = math.max(360, 95 * (depth + 1)), height = 200, nodeRadius = 12
    )
}

case class DeepNarrowWidget() extends DHtmlComponent {
  import DeepNarrowWidget._
  import DeepNarrowNetModel.{f, approx, maxDepth}

  private var depth: Int = 3

  private def setDepth(d: Int): Unit =
    depth = math.max(1, math.min(maxDepth, d))
    rerender()

  override protected def render =
    <.div(^.cls := styling.className,

      frame.svg(
        frame.axes,
        frame.curve(f, stroke = "#aaa", strokeWidth = 1.5, dash = Some("5 4")),
        frame.curve(approx(depth), stroke = "#7c3aed", strokeWidth = 2.5)
      ),

      <.div(^.cls := "dn-info",
        s"depth $depth -> ${depth + 1} layers x 2 neurons, ${math.max(0, depth - 1)} correction term(s) so far (no training)"),

      <.div(^.cls := "dn-controls",
        <.span(^.cls := "dn-label", "depth ="),
        <("input")(
          ^.attr("type") := "range", ^.attr("min") := "1", ^.attr("max") := maxDepth.toString, ^.attr("step") := "1",
          ^.attr("value") := depth.toString,
          ^.on("input") ==> { (e: dom.Event) => setDepth(e.target.asInstanceOf[dom.html.Input].value.toInt) }
        ),
        <.span(^.cls := "dn-label", depth.toString)
      ),

      <.div(^.cls := "dn-row", NetworkView.render(diagram(depth)))
    )
}
