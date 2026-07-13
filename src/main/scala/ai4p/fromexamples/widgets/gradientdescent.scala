package ai4p.fromexamples.widgets

import com.wbillingsley.veautiful.html.{<, SVG, Styling, DHtmlComponent, ^}
import org.scalajs.dom

import scala.collection.mutable

import ai4p.{*, given}
import Common.*

import site.given


object GradientDescent {

  val w = 500
  val h = 350
  val pad = 40

  val xMin = -5.0
  val xMax = 5.0
  val yMin = -1.0
  val yMax = 26.0

  def toSvgX(x: Double): Double =
    pad + (x - xMin) / (xMax - xMin) * (w - 2 * pad)

  def toSvgY(y: Double): Double =
    (h - pad) - (y - yMin) / (yMax - yMin) * (h - 2 * pad)

  /** Clamp a value into the visible data range for drawing */
  def clampX(x: Double): Double = math.max(xMin - 1, math.min(xMax + 1, x))
  def clampY(y: Double): Double = math.max(yMin - 2, math.min(yMax + 5, y))

  val styling = Styling(
    """|display: inline-block;
       |font-family: 'Lato', sans-serif;
       |""".stripMargin
  ).modifiedBy(
    " svg" -> "background: #f8f8f8; border: 1px solid #ddd; border-radius: 4px;",
    " .gd-controls" -> "margin-top: 8px; display: flex; gap: 8px; align-items: center; flex-wrap: wrap;",
    " .gd-info" -> "font-family: 'Playfair Design', serif; font-style: italic; font-size: 1.05rem; color: #5a074f; margin-top: 6px;",
    " .gd-status" -> "font-size: 0.85rem; padding: 2px 8px; border-radius: 4px; font-weight: bold;",
    " .gd-status.converging" -> "background: #dcfce7; color: #166534;",
    " .gd-status.oscillating" -> "background: #fef9c3; color: #854d0e;",
    " .gd-status.diverging" -> "background: #fee2e2; color: #991b1b;",
    " .gd-label" -> "font-size: 0.85rem; color: #666;",
    " input[type=range]" -> "width: 120px;",
    " .gd-legend" -> "font-size: 0.8rem; color: #555; display: flex; gap: 14px; flex-wrap: wrap; margin-top: 2px;",
    " .gd-legend-item" -> "display: inline-flex; align-items: center;",
    " .gd-swatch" -> "display: inline-block; width: 12px; height: 4px; border-radius: 2px; margin-right: 5px;"
  ).register()
}

case class GradientDescent(
  initialX: Double = 3.5,
  initialLR: Double = 0.1,
  /** How much to flatten the bowl near its minimum, from 0 (plain x^2) up to
    * just under 1 (very flat middle). Use e.g. 0.6 to show ridge regression:
    * unlike scaling the whole curve, this only softens the region around the
    * minimum while matching the plain f(x) = x^2 curve out at the edges, so
    * the two curves visibly coincide away from the middle. */
  flattenAmount: Double = 0.0,
  /** Roughly how wide (in x) the flattened region around the minimum is. */
  flattenWidth: Double = 1.5
) extends DHtmlComponent {

  import GradientDescent._

  private val r2 = flattenWidth * flattenWidth

  /** The loss surface: x^2, softened near x=0 by flattenAmount and tapering
    * back to matching x^2 once |x| grows past ~flattenWidth. */
  def f(x: Double): Double =
    val x2 = x * x
    x2 - flattenAmount * x2 * r2 / (r2 + x2)

  /** Derivative of f, matching the rational flattening term above. */
  def df(x: Double): Double =
    val x2 = x * x
    val d = r2 + x2
    2.0 * x * (1.0 - flattenAmount * r2 * r2 / (d * d))

  def curvePoints(steps: Int = 200): Seq[(Double, Double)] =
    (0 to steps).map { i =>
      val x = xMin + (xMax - xMin) * i.toDouble / steps
      (x, f(x))
    }

  var lr: Double = initialLR
  var currentX: Double = initialX
  val history: mutable.ArrayBuffer[Double] = mutable.ArrayBuffer(initialX)
  var autoRunning = false
  var timerId: Option[Int] = None

  def step(): Unit =
    val grad = df(currentX)
    currentX = currentX - lr * grad
    history.append(currentX)

  /** Detect the regime based on recent history */
  def regime: String =
    if history.size < 3 then "converging"
    else
      val recent = history.takeRight(6).map(f)
      val diffs = recent.sliding(2).map { s => s(1) - s(0) }.toSeq
      val signChanges = diffs.sliding(2).count { s => s(0) * s(1) < 0 }
      if recent.last > recent.head * 1.5 && recent.last > 1.0 then "diverging"
      else if signChanges >= 2 then "oscillating"
      else "converging"

  def reset(): Unit =
    stopAuto()
    currentX = initialX
    history.clear()
    history.append(initialX)

  def stopAuto(): Unit =
    timerId.foreach(dom.window.clearInterval(_))
    timerId = None
    autoRunning = false

  def toggleAuto(): Unit =
    if autoRunning then
      stopAuto()
      rerender()
    else
      autoRunning = true
      timerId = Some(dom.window.setInterval(() => {
        step()
        // Stop automatically if it's blown up beyond any useful range
        if math.abs(currentX) > 1e6 then stopAuto()
        rerender()
      }, 400))
      rerender()

  override def afterDetach(): Unit = stopAuto()

  override protected def render =
    val curve = curvePoints()
    val polyStr = curve.map((x, y) => s"${toSvgX(x)},${toSvgY(y)}").mkString(" ")

    // Decompose into data loss (x^2) and the ridge penalty added on top of it,
    // so the combined curve above can be explained as their sum.
    val showPenaltyBreakdown = flattenAmount != 0.0
    val dataLossPolyStr = curve.map((x, _) => s"${toSvgX(x)},${toSvgY(x * x)}").mkString(" ")
    val penaltyPolyStr = curve.map((x, y) => s"${toSvgX(x)},${toSvgY(y - x * x)}").mkString(" ")

    val yAtX = f(currentX)
    val slope = df(currentX)
    val inView = math.abs(currentX) <= xMax + 1

    // Tangent line at current position (only if in view)
    val tangentHalf = 0.6
    val tx0 = currentX - tangentHalf
    val tx1 = currentX + tangentHalf
    val ty0 = yAtX + slope * (tx0 - currentX)
    val ty1 = yAtX + slope * (tx1 - currentX)

    val status = regime
    val statusLabel = status match
      case "converging"  => "Converging ✓"
      case "oscillating" => "Oscillating ⚠"
      case "diverging"   => "Diverging ✗"

    <.div(^.cls := styling.className,
      SVG.svg(^.attr("width") := w, ^.attr("height") := h,

        // Clip path so trails don't overflow
        SVG("defs")(
          SVG("clipPath")(^.attr("id") := "plotArea",
            SVG.rect(^.attr("x") := pad, ^.attr("y") := pad,
              ^.attr("width") := w - 2 * pad, ^.attr("height") := h - 2 * pad)
          ),
          SVG("marker")(
            ^.attr("id") := "arrowhead", ^.attr("markerWidth") := "8",
            ^.attr("markerHeight") := "6", ^.attr("refX") := "8", ^.attr("refY") := "3",
            ^.attr("orient") := "auto",
            SVG("polygon")(^.attr("points") := "0 0, 8 3, 0 6", ^.attr("fill") := "#22c55e")
          )
        ),

        // x-axis
        SVG.line(^.attr("x1") := pad, ^.attr("y1") := toSvgY(0),
                 ^.attr("x2") := w - pad, ^.attr("y2") := toSvgY(0),
                 ^.attr("stroke") := "#bbb", ^.attr("stroke-width") := "1"),
        // y-axis
        SVG.line(^.attr("x1") := toSvgX(0), ^.attr("y1") := pad,
                 ^.attr("x2") := toSvgX(0), ^.attr("y2") := h - pad,
                 ^.attr("stroke") := "#bbb", ^.attr("stroke-width") := "1"),

        // Axis labels
        SVG.text(^.attr("x") := w / 2, ^.attr("y") := h - 4,
                 ^.attr("text-anchor") := "middle", ^.attr("font-size") := "13",
                 ^.attr("fill") := "#666", "x"),
        SVG.text(^.attr("x") := 12, ^.attr("y") := h / 2,
                 ^.attr("text-anchor") := "middle", ^.attr("font-size") := "13",
                 ^.attr("fill") := "#666", ^.attr("transform") := s"rotate(-90,12,${h / 2})", "f(x)"),

        // Data loss and penalty breakdown (dashed), shown only when there's a penalty to explain
        if showPenaltyBreakdown then
          SVG.g(
            SVG.polyline(
              ^.attr("points") := dataLossPolyStr,
              ^.attr("fill") := "none", ^.attr("stroke") := "#94a3b8",
              ^.attr("stroke-width") := "2", ^.attr("stroke-dasharray") := "6 3"
            ),
            SVG.polyline(
              ^.attr("points") := penaltyPolyStr,
              ^.attr("fill") := "none", ^.attr("stroke") := "#f97316",
              ^.attr("stroke-width") := "2", ^.attr("stroke-dasharray") := "6 3"
            )
          )
        else <.span(),

        // Loss curve (data loss + penalty, if any)
        SVG.polyline(
          ^.attr("points") := polyStr,
          ^.attr("fill") := "none", ^.attr("stroke") := "#3b82f6",
          ^.attr("stroke-width") := "2.5"
        ),

        // Clipped group for history trail and tangent
        SVG.g(^.attr("clip-path") := "url(#plotArea)",

          // History trail segments
          for i <- history.indices.dropRight(1) yield {
            val x0 = clampX(history(i));  val y0 = clampY(f(history(i)))
            val x1 = clampX(history(i+1)); val y1 = clampY(f(history(i+1)))
            SVG.line(
              ^.attr("x1") := toSvgX(x0), ^.attr("y1") := toSvgY(y0),
              ^.attr("x2") := toSvgX(x1), ^.attr("y2") := toSvgY(y1),
              ^.attr("stroke") := (if status == "diverging" then "#ef4444" else "#5a074f"),
              ^.attr("stroke-width") := "1.5",
              ^.attr("stroke-dasharray") := "3 2", ^.attr("opacity") := "0.5"
            )
          },

          // History dots
          for hx <- history.init if math.abs(hx) <= xMax + 1 yield
            SVG.circle(
              ^.attr("cx") := toSvgX(clampX(hx)), ^.attr("cy") := toSvgY(clampY(f(hx))),
              ^.attr("r") := "3", ^.attr("fill") := "#5a074f", ^.attr("opacity") := "0.4"
            ),

          // Tangent line (only when in view)
          if inView then SVG.line(
            ^.attr("x1") := toSvgX(clampX(tx0)), ^.attr("y1") := toSvgY(clampY(ty0)),
            ^.attr("x2") := toSvgX(clampX(tx1)), ^.attr("y2") := toSvgY(clampY(ty1)),
            ^.attr("stroke") := "#f59e0b", ^.attr("stroke-width") := "2",
            ^.attr("stroke-dasharray") := "5 3"
          ) else <.span(),

          // Gradient arrow
          if inView then
            val nextX = currentX - lr * slope
            SVG.line(
              ^.attr("x1") := toSvgX(clampX(currentX)), ^.attr("y1") := toSvgY(clampY(yAtX)) + 14,
              ^.attr("x2") := toSvgX(clampX(nextX)), ^.attr("y2") := toSvgY(clampY(yAtX)) + 14,
              ^.attr("stroke") := "#22c55e", ^.attr("stroke-width") := "2",
              ^.attr("marker-end") := "url(#arrowhead)"
            )
          else <.span()
        ),

        // Current position dot (or off-screen indicator)
        if inView then
          SVG.circle(
            ^.attr("cx") := toSvgX(clampX(currentX)), ^.attr("cy") := toSvgY(clampY(yAtX)),
            ^.attr("r") := "6", ^.attr("fill") := (if status == "diverging" then "#ef4444" else "#ef4444"),
            ^.attr("stroke") := "white", ^.attr("stroke-width") := "2"
          )
        else
          // Arrow pointing off-screen in the direction the ball went
          val edgeX = if currentX > 0 then w - pad - 5 else pad + 5
          SVG.text(^.attr("x") := edgeX, ^.attr("y") := pad + 15,
            ^.attr("text-anchor") := (if currentX > 0 then "end" else "start"),
            ^.attr("font-size") := "18", ^.attr("fill") := "#ef4444",
            if currentX > 0 then "→ off screen" else "← off screen"
          )
      ),

      <.div(^.cls := "gd-info",
        (if showPenaltyBreakdown then "loss = data loss + penalty" else "f(x) = x²") + "   —   " + (
          if math.abs(currentX) < 1e6 then
            f"x = $currentX%.4f,  f(x) = $yAtX%.4f,  f′(x) = $slope%.4f   (step ${history.size - 1})"
          else
            s"x has diverged to ${if currentX > 0 then "+∞" else "−∞"}   (step ${history.size - 1})"
        )
      ),

      if showPenaltyBreakdown then
        <.div(^.cls := "gd-legend",
          <.span(^.cls := "gd-legend-item", <.span(^.cls := "gd-swatch", ^.style := "background: #3b82f6;"), " combined loss (what GD follows)"),
          <.span(^.cls := "gd-legend-item", <.span(^.cls := "gd-swatch", ^.style := "background: #94a3b8;"), " data loss (x²)"),
          <.span(^.cls := "gd-legend-item", <.span(^.cls := "gd-swatch", ^.style := "background: #f97316;"), " ridge penalty")
        )
      else <.span(),

      <.div(^.cls := "gd-controls",
        <.button(^.cls := "btn btn-outline-primary btn-sm",
          ^.onClick --> { step(); rerender() }, "Step"),
        <.button(^.cls := (if autoRunning then "btn btn-warning btn-sm" else "btn btn-outline-success btn-sm"),
          ^.onClick --> toggleAuto(),
          if autoRunning then "Stop" else "Auto"),
        <.button(^.cls := "btn btn-outline-secondary btn-sm",
          ^.onClick --> { reset(); rerender() }, "Reset"),

        <.span(^.cls := "gd-label", "α ="),
        <("input")(
          ^.attr("type") := "range",
          ^.attr("min") := "0.01", ^.attr("max") := "1.5", ^.attr("step") := "0.01",
          ^.attr("value") := lr.toString,
          ^.on("input") ==> { (e: dom.Event) =>
            lr = e.target.asInstanceOf[dom.html.Input].value.toDouble
            rerender()
          }
        ),
        <.span(^.cls := "gd-label", f"$lr%.2f"),

        <.span(^.cls := s"gd-status $status", statusLabel)
      )
    )
}