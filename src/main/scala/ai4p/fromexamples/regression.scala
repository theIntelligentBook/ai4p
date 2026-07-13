package ai4p.fromexamples
import com.wbillingsley.veautiful.html.*
import com.wbillingsley.veautiful.doctacular.DeckBuilder

import <._
import ^._

import ai4p.{*, given}
import Common.*

import site.given
import scala.util.Random

import widgets.*  
import coderunner.JSCodable
import canvasland.CanvasLand
import canvasland.LineTurtle
import coderunner.PrefabCodable

object LinearRegressionWidget {

  val w = 500 * 1.5 
  val h = 350 * 1.5
  val pad = 40

  val styling = Styling(
    """|display: inline-block;
       |font-family: 'Lato', sans-serif;
       |""".stripMargin
  ).modifiedBy(
    " svg"         -> "background: #f8f8f8; border: 1px solid #ddd; border-radius: 4px; cursor: crosshair;",
    " .lr-controls"-> "margin-top: 8px; display: flex; gap: 8px; align-items: center;",
    " .lr-equation" -> "font-family: 'Playfair Design', serif; font-style: italic; font-size: 1.1rem; color: #5a074f; margin-top: 6px;"
  ).register()

  /** Map a data x value (0..1) to SVG x coordinate */
  def toSvgX(x: Double): Double = pad + x * (w - 2 * pad)
  /** Map a data y value (0..1) to SVG y coordinate (y=0 at bottom) */
  def toSvgY(y: Double): Double = (h - pad) - y * (h - 2 * pad)

  /** Ordinary least squares: returns (slope, intercept) */
  def ols(pts: Seq[(Double, Double)]): (Double, Double) =
    if pts.size < 2 then (0.0, 0.5)
    else
      val n  = pts.size.toDouble
      val mx = pts.map(_._1).sum / n
      val my = pts.map(_._2).sum / n
      val ss = pts.map { case (x, _) => (x - mx) * (x - mx) }.sum
      val sp = pts.map { case (x, y) => (x - mx) * (y - my) }.sum
      val slope = if ss == 0 then 0.0 else sp / ss
      val intercept = my - slope * mx
      (slope, intercept)

}

case class LinearRegressionWidget() extends DHtmlComponent {

  import LinearRegressionWidget._

  // Points stored in data space [0,1] x [0,1]
  var points: Seq[(Double, Double)] = Seq(
    (0.1, 0.2), (0.3, 0.35), (0.5, 0.5), (0.7, 0.6), (0.9, 0.85)
  )

  /** Convert a mouse event on the SVG to data coordinates */
  def addPoint(e: org.scalajs.dom.MouseEvent): Unit = {
    val rect = e.currentTarget.asInstanceOf[org.scalajs.dom.svg.SVG].getBoundingClientRect()
    // rect is the on-screen (post CSS-transform) size, which may be scaled
    // relative to the SVG's own w x h coordinate space (e.g. inside a scaled slide deck)
    val svgX = (e.clientX - rect.left) * w / rect.width
    val svgY = (e.clientY - rect.top) * h / rect.height
    val px = (svgX - pad) / (w - 2 * pad)
    val py = 1.0 - (svgY - pad) / (h - 2 * pad)
    if px >= 0 && px <= 1 && py >= 0 && py <= 1 then
      points = points :+ (px, py)
    rerender()
  }

  override protected def render =
    val (slope, intercept) = ols(points)

    // Regression line endpoints in SVG space
    val x0 = 0.0;  val y0 = slope * x0 + intercept
    val x1 = 1.0;  val y1 = slope * x1 + intercept

    val slopeStr     = f"$slope%.3f"
    val interceptStr = f"$intercept%.3f"

    <.div(^.cls := styling.className,
      SVG.svg(
        ^.attr("width")  := w, ^.attr("height") := h,
        ^.onClick ==> addPoint,

        // Axes
        SVG.line(^.attr("x1") := pad, ^.attr("y1") := h - pad,
                 ^.attr("x2") := w - pad, ^.attr("y2") := h - pad,
                 ^.attr("stroke") := "#999", ^.attr("stroke-width") := "1"),
        SVG.line(^.attr("x1") := pad, ^.attr("y1") := pad,
                 ^.attr("x2") := pad, ^.attr("y2") := h - pad,
                 ^.attr("stroke") := "#999", ^.attr("stroke-width") := "1"),

        // Axis labels
        SVG.text(^.attr("x") := w / 2, ^.attr("y") := h - 6,
                 ^.attr("text-anchor") := "middle", ^.attr("font-size") := "13",
                 ^.attr("fill") := "#666", "x"),
        SVG.text(^.attr("x") := 12, ^.attr("y") := h / 2,
                 ^.attr("text-anchor") := "middle", ^.attr("font-size") := "13",
                 ^.attr("fill") := "#666", ^.attr("transform") := s"rotate(-90,12,${h/2})", "y"),

        // Regression line
        SVG.line(
          ^.attr("x1") := toSvgX(x0), ^.attr("y1") := toSvgY(y0),
          ^.attr("x2") := toSvgX(x1), ^.attr("y2") := toSvgY(y1),
          ^.attr("stroke") := "#5a074f", ^.attr("stroke-width") := "2",
          ^.attr("stroke-dasharray") := "6 3"
        ),

        // Residual lines
        for (px, py) <- points yield
          SVG.line(
            ^.attr("x1") := toSvgX(px), ^.attr("y1") := toSvgY(py),
            ^.attr("x2") := toSvgX(px), ^.attr("y2") := toSvgY(slope * px + intercept),
            ^.attr("stroke") := "#f59e0b", ^.attr("stroke-width") := "1",
            ^.attr("stroke-dasharray") := "3 2"
          ),

        // Data points
        for (px, py) <- points yield
          SVG.circle(
            ^.attr("cx") := toSvgX(px), ^.attr("cy") := toSvgY(py),
            ^.attr("r") := "5",
            ^.attr("fill") := "#3b82f6", ^.attr("stroke") := "white",
            ^.attr("stroke-width") := "1.5"
          )
      ),

      <.div(^.cls := "lr-equation",
        s"ŷ = ${slopeStr} x + ${interceptStr}   (n = ${points.size})"
      ),

      <.div(^.cls := "lr-controls",
        <.button(^.cls := "btn btn-outline-secondary btn-sm",
          ^.onClick --> { points = Seq.empty; rerender() },
          "Clear"
        ),
        <.span(^.attr("style") := "color:#888; font-size:0.85rem;",
          "Click the chart to add points"
        )
      )
    )
}


object LogisticRegressionWidget {

  val w = 500
  val h = 350
  val pad = 40

  val styling = Styling(
    """|display: inline-block;
       |font-family: 'Lato', sans-serif;
       |""".stripMargin
  ).modifiedBy(
    " svg"          -> "background: #f8f8f8; border: 1px solid #ddd; border-radius: 4px; cursor: crosshair;",
    " .lr-controls" -> "margin-top: 8px; display: flex; gap: 8px; align-items: center; flex-wrap: wrap;",
    " .lr-equation"  -> "font-family: 'Playfair Design', serif; font-style: italic; font-size: 1.1rem; color: #5a074f; margin-top: 6px;",
    " .lr-legend"    -> "font-size: 0.8rem; color: #666; margin-top: 4px;"
  ).register()

  /** Map data x in [0,1] to SVG x */
  def toSvgX(x: Double): Double = pad + x * (w - 2 * pad)
  /** Map data y in [0,1] to SVG y (y=0 at bottom) */
  def toSvgY(y: Double): Double = (h - pad) - y * (h - 2 * pad)

  /** Logistic / sigmoid function */
  def sigmoid(z: Double): Double = 1.0 / (1.0 + math.exp(-z))

  /**
   * Fit logistic regression via gradient descent.
   * Points are (x, label) where label is 0 or 1.
   * Returns (w0, w1) such that P(y=1|x) = sigmoid(w0 + w1*x).
   */
  def fitLogistic(pts: Seq[(Double, Int)], steps: Int = 2000, lr: Double = 1.0): (Double, Double) =
    if pts.isEmpty then (0.0, 0.0)
    else
      var w0 = 0.0
      var w1 = 0.0
      val n = pts.size.toDouble
      for _ <- 0 until steps do
        var g0 = 0.0; var g1 = 0.0
        for (x, y) <- pts do
          val err = sigmoid(w0 + w1 * x) - y
          g0 += err; g1 += err * x
        w0 -= lr / n * g0
        w1 -= lr / n * g1
      (w0, w1)

}

case class LogisticRegressionWidget() extends DHtmlComponent {

  import LogisticRegressionWidget._

  // Points: (x in [0,1], class label 0 or 1)
  var points: Seq[(Double, Int)] = Seq(
    (0.1, 0), (0.2, 0), (0.35, 0), (0.4, 0), (0.45, 0),
    (0.55, 1), (0.6, 1), (0.7, 1), (0.8, 1), (0.9, 1)
  )

  // Which class the next click will place
  var activeClass: Int = 1

  def addPoint(e: org.scalajs.dom.MouseEvent): Unit =
    val rect = e.currentTarget.asInstanceOf[org.scalajs.dom.svg.SVG].getBoundingClientRect()
    // rect is the on-screen (post CSS-transform) size, which may be scaled
    // relative to the SVG's own w x h coordinate space (e.g. inside a scaled slide deck)
    val svgX = (e.clientX - rect.left) * w / rect.width
    val svgY = (e.clientY - rect.top) * h / rect.height
    val px = (svgX - pad) / (w - 2 * pad)
    val py = 1.0 - (svgY - pad) / (h - 2 * pad)
    if px >= 0 && px <= 1 && py >= 0 && py <= 1 then
      points = points :+ (px, activeClass)
    rerender()

  override protected def render =
    val (b0, b1) = fitLogistic(points)

    // Decision boundary: sigmoid(b0 + b1*x) = 0.5  =>  x = -b0/b1
    val boundaryX = if b1 != 0 then Some(-b0 / b1) else None

    val b0Str = f"$b0%.2f"
    val b1Str = f"$b1%.2f"

    // Sigmoid curve — sample 80 points across x in [0,1]
    val curvePoints =
      (0 to 80).map { i =>
        val x = i / 80.0
        val y = sigmoid(b0 + b1 * x)
        s"${toSvgX(x)},${toSvgY(y)}"
      }.mkString(" ")

    val boundaryLine = boundaryX match
      case Some(bx) if bx >= 0 && bx <= 1 =>
        SVG.line(
          ^.attr("x1") := toSvgX(bx), ^.attr("y1") := toSvgY(0.0),
          ^.attr("x2") := toSvgX(bx), ^.attr("y2") := toSvgY(1.0),
          ^.attr("stroke") := "#f59e0b", ^.attr("stroke-width") := "1.5",
          ^.attr("stroke-dasharray") := "6 3"
        )
      case _ => SVG.g()

    <.div(^.cls := styling.className,
      SVG.svg(
        ^.attr("width")  := w,
        ^.attr("height") := h,
        ^.onClick ==> addPoint,

        // Axes
        SVG.line(^.attr("x1") := pad,     ^.attr("y1") := h - pad,
                 ^.attr("x2") := w - pad, ^.attr("y2") := h - pad,
                 ^.attr("stroke") := "#999", ^.attr("stroke-width") := "1"),
        SVG.line(^.attr("x1") := pad, ^.attr("y1") := pad,
                 ^.attr("x2") := pad, ^.attr("y2") := h - pad,
                 ^.attr("stroke") := "#999", ^.attr("stroke-width") := "1"),

        // y=0.5 reference line
        SVG.line(
          ^.attr("x1") := pad,     ^.attr("y1") := toSvgY(0.5),
          ^.attr("x2") := w - pad, ^.attr("y2") := toSvgY(0.5),
          ^.attr("stroke") := "#ccc", ^.attr("stroke-width") := "1",
          ^.attr("stroke-dasharray") := "4 3"
        ),
        SVG.text(^.attr("x") := pad - 4, ^.attr("y") := toSvgY(0.5) + 4,
                 ^.attr("text-anchor") := "end", ^.attr("font-size") := "11",
                 ^.attr("fill") := "#aaa", "0.5"),

        // Axis labels
        SVG.text(^.attr("x") := w / 2, ^.attr("y") := h - 6,
                 ^.attr("text-anchor") := "middle", ^.attr("font-size") := "13",
                 ^.attr("fill") := "#666", "x"),
        SVG.text(^.attr("x") := 12, ^.attr("y") := h / 2,
                 ^.attr("text-anchor") := "middle", ^.attr("font-size") := "13",
                 ^.attr("fill") := "#666",
                 ^.attr("transform") := s"rotate(-90,12,${h/2})", "P(y=1|x)"),

        // Sigmoid curve
        SVG.polyline(
          ^.attr("points") := curvePoints,
          ^.attr("fill") := "none",
          ^.attr("stroke") := "#5a074f",
          ^.attr("stroke-width") := "2.5"
        ),

        // Decision boundary vertical line
        boundaryLine,

        // Data points — class 0 (red circles), class 1 (blue circles)
        // drawn at y=0.05 and y=0.95 on the axis so they don't overlap the curve
        for (px, cls) <- points yield
          SVG.circle(
            ^.attr("cx") := toSvgX(px),
            ^.attr("cy") := toSvgY(if cls == 1 then 0.95 else 0.05),
            ^.attr("r")  := "6",
            ^.attr("fill")         := (if cls == 1 then "#3b82f6" else "#ef4444"),
            ^.attr("stroke")       := "white",
            ^.attr("stroke-width") := "1.5",
            ^.attr("opacity")      := "0.85"
          )
      ),

      <.div(^.cls := "lr-equation",
        s"P(y=1|x) = σ($b0Str + $b1Str · x)   (n = ${points.size})"
      ),
      <.div(^.cls := "lr-legend",
        "Blue = class 1  ·  Red = class 0  ·  Amber dashed = decision boundary"
      ),

      <.div(^.cls := "lr-controls",
        <.span("Place:"),
        <.button(
          ^.cls := (if activeClass == 1 then "btn btn-primary btn-sm" else "btn btn-outline-primary btn-sm"),
          ^.onClick --> { activeClass = 1; rerender() },
          "Class 1 (blue)"
        ),
        <.button(
          ^.cls := (if activeClass == 0 then "btn btn-danger btn-sm" else "btn btn-outline-danger btn-sm"),
          ^.onClick --> { activeClass = 0; rerender() },
          "Class 0 (red)"
        ),
        <.button(
          ^.cls := "btn btn-outline-secondary btn-sm",
          ^.onClick --> { points = Seq.empty; rerender() },
          "Clear"
        )
      )
    )
}

val stabilityStyling = Styling(
  """|display: block;
     |""".stripMargin
).modifiedBy(
  " > div:last-child" -> "display: flex; flex-direction: row; align-items: flex-start; gap: 16px; flex-wrap: wrap;"
).register()

val regression = DeckBuilder(1920, 1080)
  .markdownSlide(
    """
      |# Regression
      |
      |""".stripMargin
  ).withClass("center middle")
  .markdownSlides(
    """
    |## Fitting functions to data
    |
    |Most decisions by a computer can be thought of as a mathematical function.
    |
    |So, when we ask a computer to learn, we are asking it to find a function that can be used to describe that data
    |
    |Let's start with a simple one: what if the function is a straight line?
    |
    |
    |---
    |
    |## Linear regression and loss functions
    |
    |We're going to take some data points and try to draw the best line we can through them.
    |
    |How do we decide what's best though?
    |
    |Somehow we have to consider the amount of *error* - how far away the points are form the line we've come up with
    |
    |""".stripMargin)
  .veautifulSlide(<.div(
    <.h2("Linear regression"),
    <.p(LinearRegressionWidget())
  ))
  .markdownSlides(
    """
    |## Loss function
    |
    |The "loss function" is the amount of error between our line and the data. 
    |
    |Typically, it's "mean squared error"
    |
    |* For every data point:
    |
    |  - take the difference between what our line would *predict* the value as, and what the real value was
    |  - square it (so it's positive) 
    |  - now take the mean 
    |
    |Now we just have to find the line that has the lowest mean squared error. 
    |
    |For small amounts of data, that can just be done as an equation
    |
    |""".stripMargin)
  .veautifulSlide(<.div(
    <.h2("Linear regression"),
    <.p(LinearRegressionWidget())
  ))
    .markdownSlides(
    """
    |
    |## Mutliple Linear Regression
    |
    |Often, we'll have data that depends on more than one variable.
    |
    |In which case we might want to fit a model where we assume it's some linear combination of the values.
    |
    |<table>
    |  <thead>
    |    <tr>
    |      <th>Study Hours/week</th>
    |      <th>Tutorials Attended</th>
    |      <th>Grade (%)</th>
    |    </tr>
    |  </thead>
    |  <tbody>
    |    <tr><td>2</td><td>1</td><td>42</td></tr>
    |    <tr><td>4</td><td>1</td><td>52</td></tr>
    |    <tr><td>2</td><td>4</td><td>56</td></tr>
    |    <tr><td>4</td><td>4</td><td>66</td></tr>
    |    <tr><td>6</td><td>4</td><td>76</td></tr>
    |    <tr><td>6</td><td>7</td><td>86</td></tr>
    |  </tbody>
    |</table>
    |
    |This might fit as "Grade = 30 + 5 × Study Hours + 4 × Tutorials Attended"
    |
    |But we're likely to have a few problems...
    |
    |---
    |
    |## Successive approximation
    |
    |When the data starts getting too big (e.g. has a lot of dimensions), the equation becomes too big to just "work out".
    |
    |Instead, we find ourselves having to use a process of *successive approximation* to work out the coefficients against each variable:
    |
    |* Take a guess
    |
    |* Find out you're wrong, but also which direction you're wrong in
    |
    |* Take another guess in that direction.
    |
    |When we are iteratively "getting it wrong and correcting ourselves", we want to make sure that
    |our corrections *converge*. 
    |
    |I'm going to liken this to stability in a control system... 
    |
    |---
    |
    |## Stability
    |
    |We're going to use a tiny line-following robot, with just one sensor.
    |
    |We want it to follow the edge of a dark line.
    |
    |We'll see what happens when we move the sensor
    |
    |
    |""".stripMargin)
     .veautifulSlide(<.div(^.cls := stabilityStyling.className,
      <.h2("Sensor under the robot"),
      // markdown.div(
      //   """A closed loop control system can become unstable. A quick demonstration of this is what happens if we
      //     |*move the line sensor behind the robot*. Fortunately, having the line sensor *in front* of the robot gives
      //     |us some "damping" in our control system (the oscillations get smaller).
      //     |""".stripMargin),
      PrefabCodable(
        """addLineSensor(0, 0, 255, 255, 0) // one sensor, under the robot
          |
          |setColour("blue")
          |setCompositeMode("lighten")
          |
          |while (true) {
          |    let l = readSensor(0); // how bright is our sensor?
          |    
          |    if (l < 0.5) {
          |        // dark. We're right of the edge, so steer left
          |        left(5);   
          |    } else  {
          |        // light. We're left of the edge, so steer right
          |        right(5);  
          |    }
          |
          |    forward(1);
          |}
          |""".stripMargin,
      CanvasLand()(
        fieldSize=(920 -> 640),
        viewSize=(920 -> 640),
        r = LineTurtle(120, 90) { r => },
        setup = { (c:CanvasLand) =>
          c.fillCanvas("rgb(200,180,0)")
          c.drawGrid("rgb(200,240,240)", 25, 1)
          c.withCanvasContext { ctx =>
            ctx.strokeStyle = "rgb(60,60,60)"
            ctx.lineWidth = 40
            ctx.beginPath()
            ctx.moveTo(100, 100)
            ctx.lineTo(770, 100)
            ctx.lineTo(770, 540)
            ctx.bezierCurveTo(670, 540, 150, 200, 150, 100)
            ctx.stroke()
          }
        }
      ),
      codeStyle = Some(
        "margin: 0; width: 500px; max-height: 640px; overflow: auto; " +
        "font-size: 0.8rem; line-height: 1.4; background: #1e1e1e; color: #f1f1f1; " +
        "padding: 12px; border-radius: 6px; white-space: pre-wrap;"
      ))
    ))
    .veautifulSlide(<.div(^.cls := stabilityStyling.className,
      <.h2("Sensor 1px in front of the robot - stable and damped"),
      // markdown.div(
      //   """A closed loop control system can become unstable. A quick demonstration of this is what happens if we
      //     |*move the line sensor behind the robot*. Fortunately, having the line sensor *in front* of the robot gives
      //     |us some "damping" in our control system (the oscillations get smaller).
      //     |""".stripMargin),
      PrefabCodable(
        """addLineSensor(1, 0, 255, 255, 0) // move the sensor 1px in front
          |
          |setColour("blue")
          |setCompositeMode("lighten")
          |
          |while (true) {
          |    let l = readSensor(0); // how bright is our sensor?
          |    
          |    if (l < 0.5) {
          |        // dark. We're right of the edge, so steer left
          |        left(5);   
          |    } else  {
          |        // light. We're left of the edge, so steer right
          |        right(5);  
          |    }
          |
          |    forward(1);
          |}
          |""".stripMargin,
      CanvasLand()(
        fieldSize=(920 -> 640),
        viewSize=(920 -> 640),
        r = LineTurtle(120, 90) { r => },
        setup = { (c:CanvasLand) =>
          c.fillCanvas("rgb(200,180,0)")
          c.drawGrid("rgb(200,240,240)", 25, 1)
          c.withCanvasContext { ctx =>
            ctx.strokeStyle = "rgb(60,60,60)"
            ctx.lineWidth = 40
            ctx.beginPath()
            ctx.moveTo(100, 100)
            ctx.lineTo(770, 100)
            ctx.lineTo(770, 540)
            ctx.bezierCurveTo(670, 540, 150, 200, 150, 100)
            ctx.stroke()
          }
        }
      ),
      codeStyle = Some(
        "margin: 0; width: 500px; max-height: 640px; overflow: auto; " +
        "font-size: 0.8rem; line-height: 1.4; background: #1e1e1e; color: #f1f1f1; " +
        "padding: 12px; border-radius: 6px; white-space: pre-wrap;"
      ))
    ))
    .veautifulSlide(<.div(^.cls := stabilityStyling.className,
      <.h2("Sensor 1px behind the robot - unstable"),
      // markdown.div(
      //   """A closed loop control system can become unstable. A quick demonstration of this is what happens if we
      //     |*move the line sensor behind the robot*. Fortunately, having the line sensor *in front* of the robot gives
      //     |us some "damping" in our control system (the oscillations get smaller).
      //     |""".stripMargin),
      PrefabCodable(
        """addLineSensor(-1, 0, 255, 255, 0) // move the sensor 1px behind
          |
          |setColour("blue")
          |setCompositeMode("lighten")
          |
          |while (true) {
          |    let l = readSensor(0); // how bright is our sensor?
          |    
          |    if (l < 0.5) {
          |        // dark. We're right of the edge, so steer left
          |        left(5);   
          |    } else  {
          |        // light. We're left of the edge, so steer right
          |        right(5);  
          |    }
          |
          |    forward(1);
          |}
          |""".stripMargin,
      CanvasLand()(
        fieldSize=(920 -> 640),
        viewSize=(920 -> 640),
        r = LineTurtle(120, 80) { r => },
        setup = { (c:CanvasLand) =>
          c.fillCanvas("rgb(200,180,0)")
          c.drawGrid("rgb(200,240,240)", 25, 1)
          c.withCanvasContext { ctx =>
            ctx.strokeStyle = "rgb(60,60,60)"
            ctx.lineWidth = 40
            ctx.beginPath()
            ctx.moveTo(100, 100)
            ctx.lineTo(770, 100)
            ctx.lineTo(770, 540)
            ctx.bezierCurveTo(670, 540, 150, 200, 150, 100)
            ctx.stroke()
          }
        }
      ),
      codeStyle = Some(
        "margin: 0; width: 500px; max-height: 640px; overflow: auto; " +
        "font-size: 0.8rem; line-height: 1.4; background: #1e1e1e; color: #f1f1f1; " +
        "padding: 12px; border-radius: 6px; white-space: pre-wrap;"
      ))
    ))
  .veautifulSlide(<.div(
    <.h2("Gradient descent"),
    markdown.div("""|Gradient descent tries to work out the coefficients for each variable using successive approximation. 
                    |I'm going to show it just for one variable, but really this happens on a "hyperplane" - a shape that has as many dimensions as there are coefficients to work out.
                  |
                  |How *bad* our fit is depends on our loss function (the mean squared error).
                  |Mathematically, if we "differentiate" the loss function, we get an equation for its gradient (slope at any point).
                  |
                  |In "gradient descent", we say:
                  |
                  |* Our next guess will move in the direction of the gradient (like we're rolling downhill)
                  |* We'll step by some value (the "learning rate") multiplied by that gradient.
                  |
                  |""".stripMargin),
    <.p(GradientDescent())
  ))
  .veautifulSlide(<.div(
    <.h2("Converging and diverging"),
    markdown.div("""|If we make our learning rate too big (step too far), we can go unstable. 
                    | We find we've shot so far past the middle we've got worse, like the unstable robot.
                  |""".stripMargin),
    <.p(GradientDescent())
  ))
   .markdownSlides(
    """
    |
    |## The problem of data being correlated
    |
    |Let's go back to our study example. 
    |
    |<table>
    |  <thead>
    |    <tr>
    |      <th>Study Hours/week</th>
    |      <th>Tutorials Attended</th>
    |      <th>Grade (%)</th>
    |    </tr>
    |  </thead>
    |  <tbody>
    |    <tr><td>2</td><td>1</td><td>42</td></tr>
    |    <tr><td>4</td><td>1</td><td>52</td></tr>
    |    <tr><td>2</td><td>4</td><td>56</td></tr>
    |    <tr><td>4</td><td>4</td><td>66</td></tr>
    |    <tr><td>6</td><td>4</td><td>76</td></tr>
    |    <tr><td>6</td><td>7</td><td>86</td></tr>
    |  </tbody>
    |</table>
    |
    |The chances are that study hours and tutorials attended are actually pretty correlated. They're not independent but depend on
    |the student's time and conscientiousness. We've got one dimension appearing as if it's two dimensions, making it no longer really linear.
    |
    |We could find that if we trained on data that included these (as well as independent variables, e.g. age)
    |we would "overfit" on this data. It would get too sensitive to how particular data points *happened* to vary in those two dimensions
    |and try too hard follow every random wiggle in the data set.
    |
    |""".stripMargin)
    .veautifulSlide(<.div(
    <.h2("Ridge regression"),
    markdown.div("""|Ridge regression tries to deal with correlated data by making the model less sensitive to small errors.
                    |
                    |It gives a penalty (called lambda) to having a high coefficient in any given parameter
                    |
                    |That flattens the gradient descent curve, making it less sensitive to values being off by a little, so long as they're close.
                  |""".stripMargin),
    <.p(
      GradientDescent(), " ",
      (GradientDescent(flattenAmount = 0.7, flattenWidth = 3))
    )
  ))
  .markdownSlides(
    """
    |
    |## Categorical data
    |
    |Let's go back to our study example. 
    |
    |<table>
    |  <thead>
    |    <tr>
    |      <th>Study Hours/week</th>
    |      <th>Tutorials Attended</th>
    |      <th>Grade (%)</th>
    |    </tr>
    |  </thead>
    |  <tbody>
    |    <tr><td>2</td><td>1</td><td>42</td></tr>
    |    <tr><td>4</td><td>1</td><td>52</td></tr>
    |    <tr><td>2</td><td>4</td><td>56</td></tr>
    |    <tr><td>4</td><td>4</td><td>66</td></tr>
    |    <tr><td>6</td><td>4</td><td>76</td></tr>
    |    <tr><td>6</td><td>7</td><td>86</td></tr>
    |  </tbody>
    |</table>
    |
    |That works ok for your *mark*, but what if what we care about is whether you *drop out*. Or even whether you *fail* (which might depend on more than your mark, e.g. did you do all the compulsory stuff?)
    |
    |Those sorts of decisions are yes/no decisions, not a number, so we can't fit a straight line to them.
    |
    |""".stripMargin)
  .veautifulSlide(<.div(
    <.h2("Logistic regression"),
    markdown.div("""|Sometimes, we're trying to predict something that's *true* or *false* rather than having a continuous value.
                    |
                    |In these cases, we try to fit a function to the *probability* that you'll be in one of the classes (failing or passing)
                    |
                    |Because probabilities are always between 0 and 1, they don't fit a linear equation. So instead we fit it to a "sigmoid" 
                  |""".stripMargin),
    <.p(LogisticRegressionWidget())
  ))
  .markdownSlide(willCcBy)
  .renderSlides
