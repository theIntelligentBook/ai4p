package ai4p.embeddings.widgets

import com.wbillingsley.veautiful.html.{<, SVG, Styling, DHtmlComponent, DSvgContent, ^}
import org.scalajs.dom
import ai4p.{*, given}
import ai4p.neuralnets.widgets.PlotFrame

/**
 * Ties "a matrix transforms a vector" back to something you can literally see: pick a 2x2 matrix
 * from the dropdown and watch it drag a simple shape into a new one. Click any corner of either
 * shape to see that point's own vector, before and after.
 *
 * The shape is the letter "F" - deliberately asymmetric, so a rotation, reflection or shear each
 * produce a visibly different (and not accidentally-still-the-same-looking) result.
 */
object AffineTransformWidget {

  case class Transform(name: String, a: Double, b: Double, c: Double, d: Double)

  private val r45 = math.Pi / 4
  val transforms: Vector[Transform] = Vector(
    Transform("Identity", 1, 0, 0, 1),
    Transform("Scale ×2", 2, 0, 0, 2),
    Transform("Scale ×0.5", 0.5, 0, 0, 0.5),
    Transform("Stretch x ×2", 2, 0, 0, 1),
    Transform("Rotate 45°", math.cos(r45), -math.sin(r45), math.sin(r45), math.cos(r45)),
    Transform("Rotate 90°", 0, -1, 1, 0),
    Transform("Reflect over x-axis", 1, 0, 0, -1),
    Transform("Reflect over y-axis", -1, 0, 0, 1),
    Transform("Shear x", 1, 0.5, 0, 1),
    Transform("Shear y", 1, 0, 0.5, 1)
  )

  /** An "F", traced as one outline - asymmetric under both axes, so every transform below gives
    * a visibly distinct result (a square or circle would look the same after a reflection). */
  val shape: Vector[(Double, Double)] = Vector(
    (0, 0), (0.5, 0), (0.5, 1.4), (1.2, 1.4), (1.2, 1.9), (0.5, 1.9), (0.5, 2.5), (1.5, 2.5), (1.5, 3), (0, 3)
  )

  def transformPoint(t: Transform, p: (Double, Double)): (Double, Double) =
    val (x, y) = p
    (t.a * x + t.b * y, t.c * x + t.d * y)

  val styling = Styling(
    """|display: inline-block;
       |font-family: 'Lato', sans-serif;
       |""".stripMargin
  ).modifiedBy(
    " .at-controls" -> "margin-bottom: 8px; display: flex; align-items: center; gap: 8px; flex-wrap: wrap;",
    " select" -> "font-size: 0.85rem; padding: 2px 4px;",
    " .at-matrix" -> "display: inline-flex; align-items: stretch; margin: 6px 0 14px; font-family: monospace; font-size: 0.95rem;",
    " .at-bracket-l" -> "width: 6px; border-left: 2px solid #333; border-top: 2px solid #333; border-bottom: 2px solid #333; margin-right: 6px;",
    " .at-bracket-r" -> "width: 6px; border-right: 2px solid #333; border-top: 2px solid #333; border-bottom: 2px solid #333; margin-left: 6px;",
    " .at-matrix-grid" -> "display: grid; grid-template-columns: repeat(2, 48px); row-gap: 4px; align-items: center;",
    " .at-matrix-grid div" -> "text-align: right; padding-right: 4px;",
    " .at-plots" -> "display: flex; gap: 24px; flex-wrap: wrap;",
    " .at-plot-col" -> "text-align: center;",
    " .at-plot-title" -> "font-weight: bold; color: #5a074f; margin-bottom: 4px;",
    " .at-point" -> "cursor: pointer;",
    " .at-info" -> "font-family: monospace; font-size: 0.9rem; margin-top: 6px; min-height: 1.3em; color: #333;",
    " .at-hint" -> "color: #666; font-size: 0.8rem; margin-top: 10px; max-width: 560px;"
  ).register()
}

case class AffineTransformWidget() extends DHtmlComponent {
  import AffineTransformWidget._

  private var transformIdx: Int = transforms.indexWhere(_.name == "Rotate 45°")
  private var selected: Option[Int] = Some(shape.size - 2) // the F's top-right corner - moves interestingly under every transform

  private def setTransform(i: Int): Unit = { transformIdx = i; rerender() }
  private def select(i: Int): Unit = { selected = Some(i); rerender() }

  private val frame = PlotFrame(xMin = -3, xMax = 6, yMin = -3, yMax = 6, w = 280, h = 280)

  private def plotShape(pts: Vector[(Double, Double)], color: String): DSvgContent =
    val poly = pts.map((x, y) => s"${frame.toSvgX(x)},${frame.toSvgY(y)}").mkString(" ")
    SVG.g(
      SVG.polygon(^.attr("points") := poly, ^.attr("fill") := color, ^.attr("fill-opacity") := "0.3",
        ^.attr("stroke") := color, ^.attr("stroke-width") := "2"),
      SVG.g(
        pts.zipWithIndex.map((p, i) =>
          val (x, y) = p
          val isSel = selected.contains(i)
          SVG.circle(^.cls := "at-point",
            ^.attr("cx") := frame.toSvgX(x), ^.attr("cy") := frame.toSvgY(y),
            ^.attr("r") := (if isSel then 6 else 4),
            ^.attr("fill") := (if isSel then "#1d4ed8" else color),
            ^.on("click") ==> { (_: dom.Event) => select(i) })
        )
      )
    )

  private def plotPanel(title: String, pts: Vector[(Double, Double)], color: String) =
    <.div(^.cls := "at-plot-col",
      <.div(^.cls := "at-plot-title", title),
      frame.svg(frame.axes, plotShape(pts, color)),
      <.div(^.cls := "at-info",
        selected.map(i => { val (x, y) = pts(i); f"($x%.2f, $y%.2f)" }).getOrElse(""))
    )

  private def matrixDisplay(t: Transform) =
    <.div(^.cls := "at-matrix",
      <.div(^.cls := "at-bracket-l"),
      <.div(^.cls := "at-matrix-grid",
        <.div(f"${t.a}%.2f"), <.div(f"${t.b}%.2f"),
        <.div(f"${t.c}%.2f"), <.div(f"${t.d}%.2f")
      ),
      <.div(^.cls := "at-bracket-r")
    )

  override protected def render =
    val t = transforms(transformIdx)
    val outputPts = shape.map(p => transformPoint(t, p))
    <.div(^.cls := styling.className,
      <.div(^.cls := "at-controls",
        "transform:",
        <.select(
          ^.on("change") ==> { (e: dom.Event) => setTransform(e.target.asInstanceOf[dom.html.Select].value.toInt) },
          for (tr, i) <- transforms.zipWithIndex yield
            <.option(^.attr("value") := i.toString, ^.attr("selected") ?= (if i == transformIdx then Some("selected") else None), tr.name)
        )
      ),
      matrixDisplay(t),
      <.div(^.cls := "at-plots",
        plotPanel("Input", shape, "#3b82f6"),
        plotPanel("Output", outputPts, "#16a34a")
      ),
      <.div(^.cls := "at-hint",
        "Click a corner of either shape to see its own vector - the matrix turns every point in the input shape into the matching point in the output shape.")
    )
}
