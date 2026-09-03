package ai4p.neuralnets.widgets

import com.wbillingsley.veautiful.html.{SVG, Styling, DSvgContent, ^}

import ai4p.{*, given}

object PlotFrame {
  val styling = Styling(
    """|background: #f8f8f8;
       |border: 1px solid #ddd;
       |border-radius: 4px;
       |""".stripMargin
  ).register()
}

/**
 * A small reusable "axes + curve" plotting helper, shared by the function-approximation
 * widgets (harmonics, Taylor series, Riemann sums, universal approximation, ...) so each one
 * doesn't have to re-derive its own coordinate mapping and axis-drawing code.
 */
case class PlotFrame(xMin: Double, xMax: Double, yMin: Double, yMax: Double, w: Int = 520, h: Int = 320, pad: Int = 30) {

  def toSvgX(x: Double): Double = pad + (x - xMin) / (xMax - xMin) * (w - 2 * pad)
  def toSvgY(y: Double): Double = (h - pad) - (y - yMin) / (yMax - yMin) * (h - 2 * pad)

  def clampY(y: Double): Double = math.max(yMin - (yMax - yMin), math.min(yMax + (yMax - yMin), y))

  /** x/y axis lines (drawn through y=0 / x=0 if they're in range, otherwise along the frame edge). */
  def axes: DSvgContent =
    val axisY = toSvgY(math.max(yMin, math.min(yMax, 0.0)))
    val axisX = toSvgX(math.max(xMin, math.min(xMax, 0.0)))
    SVG.g(
      SVG.line(^.attr("x1") := pad, ^.attr("y1") := axisY, ^.attr("x2") := w - pad, ^.attr("y2") := axisY,
        ^.attr("stroke") := "#bbb", ^.attr("stroke-width") := "1"),
      SVG.line(^.attr("x1") := axisX, ^.attr("y1") := pad, ^.attr("x2") := axisX, ^.attr("y2") := h - pad,
        ^.attr("stroke") := "#bbb", ^.attr("stroke-width") := "1")
    )

  /** A polyline tracing `f` across the frame's x-range. */
  def curve(f: Double => Double, stroke: String = "#3b82f6", strokeWidth: Double = 2.5,
            dash: Option[String] = None, steps: Int = 240): DSvgContent =
    val pts = (0 to steps).map { i =>
      val x = xMin + (xMax - xMin) * i.toDouble / steps
      s"${toSvgX(x)},${toSvgY(clampY(f(x)))}"
    }.mkString(" ")
    SVG.polyline(
      ^.attr("points") := pts, ^.attr("fill") := "none",
      ^.attr("stroke") := stroke, ^.attr("stroke-width") := strokeWidth,
      ^.attr("stroke-dasharray") := dash.getOrElse("none")
    )

  /** A filled rectangle in data-space, e.g. for drawing a Riemann-sum bar. */
  def rect(x0: Double, x1: Double, y0: Double, y1: Double, fill: String, opacity: Double = 0.35, stroke: String = "#555"): DSvgContent =
    val sx0 = toSvgX(x0); val sx1 = toSvgX(x1)
    val sy0 = toSvgY(clampY(y0)); val sy1 = toSvgY(clampY(y1))
    SVG.rect(
      ^.attr("x") := math.min(sx0, sx1), ^.attr("y") := math.min(sy0, sy1),
      ^.attr("width") := math.abs(sx1 - sx0), ^.attr("height") := math.abs(sy1 - sy0),
      ^.attr("fill") := fill, ^.attr("fill-opacity") := opacity,
      ^.attr("stroke") := stroke, ^.attr("stroke-width") := "1"
    )

  def dot(x: Double, y: Double, r: Double = 3.5, fill: String = "#1d4ed8"): DSvgContent =
    SVG.circle(^.attr("cx") := toSvgX(x), ^.attr("cy") := toSvgY(clampY(y)), ^.attr("r") := r, ^.attr("fill") := fill)

  /** Wraps a sequence of plot elements in a standalone `<svg>` sized to this frame. */
  def svg(content: DSvgContent*): DSvgContent =
    SVG.svg(^.cls := PlotFrame.styling.className, ^.attr("width") := w, ^.attr("height") := h)(content*)
}
