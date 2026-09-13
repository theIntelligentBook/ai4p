package ai4p.embeddings.widgets

import com.wbillingsley.veautiful.html.{<, SVG, Styling, DSvgContent, VHtmlContent, ^}
import ai4p.{*, given}

/**
 * A static box diagram of one transformer block: self-attention (mixing information *between*
 * tokens) feeding a per-token feed-forward network (the same kind of SwiGLU layer covered in the
 * neural-networks deck), each wrapped in a residual "add & norm". Purely illustrative layout - no
 * live computation - stacked blocks are what turn a handful of these into a full model.
 */
object TransformerBlockDiagram {

  val styling = Styling(
    """|display: inline-block;
       |font-family: 'Lato', sans-serif;
       |""".stripMargin
  ).register()

  private case class Box(label: String, sub: String, y: Double, height: Double, fill: String)

  private val w = 540
  private val boxW = 390
  private val x0 = (w - boxW) / 2.0

  // Every box needs room for two lines of text (title + subtitle) plus the gap the arrows sit in
  // below it - laid out by stacking these heights rather than hand-picking each box's y, since a
  // box that's too short for its own two lines just prints them on top of each other.
  private val topPad = 15.0
  private val gap = 24.0
  private val specs = Vector(
    ("Input embeddings", "token vector + position", 60.0, "#eef2ff"),
    ("Multi-head self-attention", "each token mixes in info from every other token", 81.0, "#dbeafe"),
    ("Add & normalise", "residual: + the block's input", 60.0, "#f3f4f6"),
    ("Feed-forward (SwiGLU)", "same transform, applied to each token on its own", 66.0, "#dcfce7"),
    ("Add & normalise", "residual: + the previous step", 60.0, "#f3f4f6")
  )

  private val boxes: Vector[Box] =
    specs.foldLeft((topPad, Vector.empty[Box])) { case ((y, acc), (label, sub, height, fill)) =>
      (y + height + gap, acc :+ Box(label, sub, y, height, fill))
    }._2

  private def arrow(x: Double, y1: Double, y2: Double): DSvgContent =
    SVG.g(
      SVG.line(^.attr("x1") := x, ^.attr("y1") := y1, ^.attr("x2") := x, ^.attr("y2") := y2,
        ^.attr("stroke") := "#999", ^.attr("stroke-width") := "2"),
      SVG.polygon(^.attr("points") := s"$x,${y2 + 7} ${x - 6},${y2 - 4} ${x + 6},${y2 - 4}", ^.attr("fill") := "#999")
    )

  private def boxSvg(b: Box): DSvgContent =
    SVG.g(
      SVG.rect(^.attr("x") := x0, ^.attr("y") := b.y, ^.attr("width") := boxW, ^.attr("height") := b.height,
        ^.attr("rx") := 9, ^.attr("fill") := b.fill, ^.attr("stroke") := "#888", ^.attr("stroke-width") := "1"),
      SVG.text(^.attr("x") := w / 2.0, ^.attr("y") := b.y + 30, ^.attr("text-anchor") := "middle",
        ^.attr("font-size") := "19", ^.attr("font-family") := "'Lato', sans-serif", ^.attr("fill") := "#222", b.label),
      SVG.text(^.attr("x") := w / 2.0, ^.attr("y") := b.y + b.height - 12, ^.attr("text-anchor") := "middle",
        ^.attr("font-size") := "14", ^.attr("font-family") := "'Lato', sans-serif", ^.attr("fill") := "#666", b.sub)
    )

  def svg: DSvgContent =
    val h = boxes.last.y + boxes.last.height + topPad
    SVG.svg(^.attr("width") := w, ^.attr("height") := h,
      SVG.g((for i <- 0 until boxes.size - 1 yield arrow(w / 2.0, boxes(i).y + boxes(i).height, boxes(i + 1).y)).toVector*),
      SVG.g(boxes.map(boxSvg)*)
    )

  def render: VHtmlContent =
    <.div(^.cls := styling.className,
      svg,
    )
}
