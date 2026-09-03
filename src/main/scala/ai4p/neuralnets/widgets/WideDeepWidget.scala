package ai4p.neuralnets.widgets

import com.wbillingsley.veautiful.html.{<, Styling, DHtmlComponent, ^}
import org.scalajs.dom

import ai4p.{*, given}

/**
 * A network with a small, fixed width - wider than [[DeepNarrowWidget]]'s strict two neurons, but
 * still much narrower than the pulses widget's single wide hidden layer - trained by ordinary
 * backprop to approximate the same wobbly target function as the pulses widget. Growing the depth
 * slider adds a whole extra layer of neurons (unlike the pulses widget, where growing meant adding
 * more neurons to one single layer), and re-training shows depth doing real work: universal
 * approximation isn't just a single-hidden-layer, width-driven story.
 */
object WideDeepWidget {
  import ApproxTarget._

  val frame = PlotFrame(xMin = xMin - 0.4, xMax = xMax + 0.4, yMin = yMin, yMax = yMax, w = 560, h = 280)
  val maxDepth = 6

  val styling = Styling(
    """|display: inline-block;
       |font-family: 'Lato', sans-serif;
       |""".stripMargin
  ).modifiedBy(
    " .wd-controls" -> "margin-top: 8px; display: flex; gap: 8px; align-items: center; flex-wrap: wrap;",
    " .wd-info" -> "font-family: monospace; font-size: 0.85rem; color: #444; margin-top: 6px;",
    " .wd-label" -> "font-size: 0.85rem; color: #666;",
    " .wd-row" -> "display: flex; gap: 24px; flex-wrap: wrap; align-items: flex-start; margin-top: 10px;",
    " input[type=range]" -> "width: 160px;"
  ).register()
}

case class WideDeepWidget() extends DHtmlComponent {
  import WideDeepWidget._
  import ApproxTarget.f
  import WideDeepNetModel.width

  private var depth: Int = 3
  private var net = new WideDeepNet(depth)
  private var autoRunning = false
  private var timerId: Option[Int] = None

  private def setDepth(d: Int): Unit =
    stopAuto()
    depth = math.max(1, math.min(maxDepth, d))
    net = new WideDeepNet(depth)
    rerender()

  private def trainBurst(n: Int = 500): Unit =
    for _ <- 1 to n do net.step()
    rerender()

  private def resetNet(): Unit =
    stopAuto()
    net = new WideDeepNet(depth, seed = (math.random() * 1e9).toLong)
    rerender()

  private def stopAuto(): Unit =
    timerId.foreach(dom.window.clearInterval(_))
    timerId = None
    autoRunning = false

  private def toggleAuto(): Unit =
    if autoRunning then
      stopAuto()
      rerender()
    else
      autoRunning = true
      timerId = Some(dom.window.setInterval(() => { net.step(); rerender() }, 80))
      rerender()

  override def afterDetach(): Unit = stopAuto()

  private def diagram: NetworkView.Diagram =
    val ls = net.layerSizes
    val scale = math.max(1e-6, net.weights.flatten.flatten.map(math.abs).max)
    NetworkView.denseDiagram(
      layerSizes = ls,
      weight = (li, from, to) => net.weights(li)(from)(to) / scale,
      layerLabels = Vector("x") ++ Vector.fill(ls.size - 2)("") ++ Vector("≈ f(x)"),
      width = math.max(360, 80 * ls.size), height = 220, nodeRadius = 10
    )

  override protected def render =
    <.div(^.cls := styling.className,

      frame.svg(
        frame.axes,
        frame.curve(f, stroke = "#aaa", strokeWidth = 1.5, dash = Some("5 4")),
        frame.curve(x => net.predict(x), stroke = "#7c3aed", strokeWidth = 2.5)
      ),

      <.div(^.cls := "wd-info",
        f"depth $depth, width $width = ${depth * width} hidden neurons   loss ${net.loss()}%.4f   epoch ${net.epoch}"),

      <.div(^.cls := "wd-controls",
        <.span(^.cls := "wd-label", "depth ="),
        <("input")(
          ^.attr("type") := "range", ^.attr("min") := "1", ^.attr("max") := maxDepth.toString, ^.attr("step") := "1",
          ^.attr("value") := depth.toString,
          ^.on("input") ==> { (e: dom.Event) => setDepth(e.target.asInstanceOf[dom.html.Input].value.toInt) }
        ),
        <.span(^.cls := "wd-label", depth.toString),
        <.button(^.cls := "btn btn-outline-primary btn-sm", ^.onClick --> trainBurst(), "Train 500 (fast)"),
        <.button(^.cls := (if autoRunning then "btn btn-warning btn-sm" else "btn btn-outline-success btn-sm"),
          ^.onClick --> toggleAuto(), if autoRunning then "Stop" else "Auto"),
        <.button(^.cls := "btn btn-outline-secondary btn-sm", ^.onClick --> resetNet(), "Reset weights")
      ),

      <.div(^.cls := "wd-row", NetworkView.render(diagram))
    )
}
