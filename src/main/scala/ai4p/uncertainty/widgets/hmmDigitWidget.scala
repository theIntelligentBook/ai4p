package ai4p.uncertainty.widgets

import com.wbillingsley.veautiful.html.{<, SVG, Styling, DHtmlComponent, VHtmlContent, VDomContent, ^}
import org.scalajs.dom
import scala.collection.mutable.ArrayBuffer

import ai4p.{*, given}
import Common.*

import site.given

object HmmDigitWidget {
  val canvasSize = 160

  val styling = Styling(
    """|display: inline-block;
       |font-family: 'Lato', sans-serif;
       |max-width: 900px;
       |""".stripMargin
  ).modifiedBy(
    " .hdw-columns" -> "display: flex; gap: 20px; flex-wrap: wrap; margin: 10px 0; align-items: flex-start;",
    " .hdw-canvas" -> "border: 2px solid #999; border-radius: 6px; background: white; touch-action: none; cursor: crosshair;",
    " .hdw-controls" -> "margin-top: 8px; display: flex; gap: 8px; align-items: center;",
    " .hdw-results" -> "flex: 1 1 320px; min-width: 260px;",
    " .hdw-placeholder" -> "color: #888; font-size: 0.9rem;",
    " .hdw-big" -> "display: flex; align-items: baseline; gap: 10px; margin-bottom: 6px;",
    " .hdw-big-digit" -> "font-size: 2.6rem; font-weight: bold; color: #1d4ed8; line-height: 1;",
    " .hdw-big-conf" -> "font-size: 0.9rem; color: #555;",
    " .hdw-section-title" -> "font-size: 0.8rem; font-weight: bold; color: #555; margin-top: 10px;",
    " .hdw-badges" -> "display: flex; gap: 4px; flex-wrap: wrap; margin: 4px 0;",
    " .hdw-badge" -> "display: flex; flex-direction: column; align-items: center; border: 1px solid #ccc; border-radius: 4px; padding: 3px 6px; font-family: monospace;",
    " .hdw-badge-glyph" -> "font-size: 1.1rem; font-weight: bold; color: #1d4ed8;",
    " .hdw-row" -> "display: flex; align-items: center; gap: 4px; margin: 4px 0;",
    " .hdw-row-label" -> "width: 108px; font-size: 0.75rem; color: #666;",
    " .hdw-cell" -> "display: inline-flex; align-items: center; justify-content: center; width: 26px; height: 26px; font-weight: bold; font-family: monospace; border-radius: 3px;",
    " .hdw-cell.hdw-mismatch" -> "background: #fee2e2; color: #991b1b;",
    " .hdw-bar-row" -> "display: flex; align-items: center; gap: 6px; margin: 2px 0; font-size: 0.85rem;",
    " .hdw-bar-row.hdw-top" -> "font-weight: bold; color: #166534;",
    " .hdw-bar-label" -> "width: 16px; text-align: center; font-weight: bold;",
    " .hdw-bar-track" -> "flex: 1; height: 10px; background: #eee; border-radius: 3px; overflow: hidden; max-width: 200px;",
    " .hdw-bar-fill" -> "height: 100%; background: #93c5fd;",
    " .hdw-bar-row.hdw-top .hdw-bar-fill" -> "background: #16a34a;",
    " .hdw-bar-pct" -> "width: 34px; font-size: 0.75rem; color: #666;",
    " .hdw-legend" -> "display: flex; gap: 10px; flex-wrap: wrap; margin-top: 12px; padding-top: 10px; border-top: 1px solid #eee;",
    " .hdw-legend-item" -> "display: flex; flex-direction: column; align-items: center; font-size: 0.7rem; color: #666;",
    " .hdw-legend-svg" -> "background: #fafafa; border: 1px solid #eee; border-radius: 4px;"
  ).register()

  /** A tiny SVG sketch of a digit's canonical chain-code shape, used as a drawing guide. */
  def legendIcon(chain: Vector[Int], size: Int = 30): VDomContent =
    import HmmDigitModel.previewPoints
    val pts = previewPoints(chain)
    val xs = pts.map(_._1); val ys = pts.map(_._2)
    val (minX, maxX) = (xs.min, xs.max)
    val (minY, maxY) = (ys.min, ys.max)
    val spanX = math.max(maxX - minX, 0.001); val spanY = math.max(maxY - minY, 0.001)
    val span = math.max(spanX, spanY)
    val pad = size * 0.18
    val scale = (size - 2 * pad) / span
    val cx = (minX + maxX) / 2; val cy = (minY + maxY) / 2
    val svgPts = pts.map((x, y) => s"${size / 2 + (x - cx) * scale},${size / 2 + (y - cy) * scale}").mkString(" ")
    SVG.svg(^.cls := "hdw-legend-svg", ^.attr("width") := size, ^.attr("height") := size,
      ^.attr("viewBox") := s"0 0 $size $size",
      SVG.polyline(^.attr("points") := svgPts, ^.attr("fill") := "none", ^.attr("stroke") := "#1d4ed8",
        ^.attr("stroke-width") := "2", ^.attr("stroke-linecap") := "round", ^.attr("stroke-linejoin") := "round")
    )
}

/**
 * Draw a digit (0-9) as a single stroke in the box. The stroke is resampled into a sequence of
 * compass-direction "strokes" (a chain code), then scored against ten small per-digit left-to-right
 * HMMs via Viterbi decoding — showing not just which digit won, but how the drawn strokes were
 * aligned to that digit's hidden states.
 */
case class HmmDigitWidget() extends DHtmlComponent {
  import HmmDigitModel._
  import HmmDigitWidget._

  private val drawCanvas = <.canvas(
    ^.cls := "hdw-canvas",
    ^.attr("width") := canvasSize,
    ^.attr("height") := canvasSize,
    ^.onMouseDown ==> onDown,
    ^.onMouseMove ==> onMove,
    ^.onMouseUp ==> onUp,
    ^.onMouseLeave ==> onUp
  ).build()

  private var isDrawing = false
  private val rawPath = ArrayBuffer.empty[(Double, Double)]

  var hasDrawn: Boolean = false
  var observations: Vector[Int] = Vector.empty
  var results: Vector[ClassificationResult] = Vector.empty

  private def context(): Option[dom.CanvasRenderingContext2D] =
    drawCanvas.domNode.map(_.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D])

  private def canvasPoint(e: dom.MouseEvent): Option[(Double, Double)] =
    drawCanvas.domNode.map { canvas =>
      val rect = canvas.getBoundingClientRect()
      val scaleX = canvas.width.toDouble / rect.width
      val scaleY = canvas.height.toDouble / rect.height
      ((e.clientX - rect.left) * scaleX, (e.clientY - rect.top) * scaleY)
    }

  private def clearCanvas(): Unit =
    for ctx <- context() do ctx.clearRect(0, 0, canvasSize, canvasSize)

  private def drawSegment(from: (Double, Double), to: (Double, Double)): Unit =
    for ctx <- context() do
      ctx.strokeStyle = "#1d4ed8"
      ctx.lineWidth = 5
      ctx.lineCap = "round"
      ctx.lineJoin = "round"
      ctx.beginPath()
      ctx.moveTo(from._1, from._2)
      ctx.lineTo(to._1, to._2)
      ctx.stroke()

  private def onDown(e: dom.MouseEvent): Unit =
    for p <- canvasPoint(e) do
      clearCanvas()
      rawPath.clear()
      rawPath += p
      isDrawing = true
      hasDrawn = false
      observations = Vector.empty
      results = Vector.empty
      rerender()

  private def onMove(e: dom.MouseEvent): Unit =
    if isDrawing then
      for p <- canvasPoint(e) do
        drawSegment(rawPath.last, p)
        rawPath += p

  private def onUp(e: dom.MouseEvent): Unit =
    if isDrawing then
      isDrawing = false
      if rawPath.size > 1 then
        val obs = toDirections(rawPath.toVector)
        if obs.nonEmpty then
          observations = obs
          results = classify(obs)
          hasDrawn = true
      rerender()

  def clear(): Unit =
    clearCanvas()
    rawPath.clear()
    isDrawing = false
    hasDrawn = false
    observations = Vector.empty
    results = Vector.empty
    rerender()

  private def glyphBadges(dirs: Vector[Int]): VHtmlContent =
    <.div(^.cls := "hdw-badges",
      for d <- dirs yield
        <.div(^.cls := "hdw-badge", <.span(^.cls := "hdw-badge-glyph", glyphs(d)))
    )

  /** Two aligned rows, like the Morse example's true/signal/decoded rows: what you actually drew,
   *  versus the direction each Viterbi-aligned hidden state of the winning digit expected — so you
   *  can see exactly where (if anywhere) your strokes and the model's idea of the shape disagree. */
  private def alignmentRows(observed: Vector[Int], statePath: Vector[Int], chain: Vector[Int]): VHtmlContent =
    val expected = statePath.map(chain)
    <.div(
      <.div(^.cls := "hdw-row",
        <.span(^.cls := "hdw-row-label", "you drew"),
        for d <- observed yield <.span(^.cls := "hdw-cell", glyphs(d))
      ),
      <.div(^.cls := "hdw-row",
        <.span(^.cls := "hdw-row-label", "state expected"),
        for (d, i) <- expected.zipWithIndex yield
          <.span(^.cls := s"hdw-cell${if d != observed(i) then " hdw-mismatch" else ""}", glyphs(d))
      )
    )

  private def rankingBars(results: Vector[ClassificationResult]): VHtmlContent =
    val confs = confidences(results)
    <.div(
      for ((r, p), i) <- results.zip(confs).zipWithIndex yield
        <.div(^.cls := s"hdw-bar-row${if i == 0 then " hdw-top" else ""}",
          <.span(^.cls := "hdw-bar-label", r.template.digit.toString),
          <.div(^.cls := "hdw-bar-track", <.div(^.cls := "hdw-bar-fill", ^.style := s"width: ${(p * 100).round}%;")),
          <.span(^.cls := "hdw-bar-pct", s"${(p * 100).round}%")
        )
    )

  override protected def render =
    <.div(^.cls := styling.className,
      <.p(^.cls := "br-label",
        "Draw a digit 0-9 as a single stroke (don't lift the mouse) — roughly following the shapes " +
        "in the guide below. On release, the stroke is turned into a sequence of compass-direction " +
        "\"strokes\", which is Viterbi-decoded against a small per-digit HMM to find both the best-" +
        "matching digit and how each stroke aligns to that digit's hidden states."),

      <.div(^.cls := "hdw-columns",
        <.div(
          drawCanvas,
          <.div(^.cls := "hdw-controls",
            <.button(^.cls := "btn btn-outline-primary btn-sm", ^.onClick --> clear(), "Clear")
          )
        ),

        <.div(^.cls := "hdw-results",
          if !hasDrawn then
            <.p(^.cls := "hdw-placeholder", "Draw a digit in the box to see it recognised.")
          else
            val best = results.head
            val conf = confidences(results).head
            <.div(
              <.div(^.cls := "hdw-big",
                <.span(^.cls := "hdw-big-digit", best.template.digit.toString),
                <.span(^.cls := "hdw-big-conf", f"${conf * 100}%.0f%% confidence")
              ),

              <.div(^.cls := "hdw-section-title", "Sequence of strokes extracted from your drawing:"),
              glyphBadges(observations),

              <.div(^.cls := "hdw-section-title", s"Aligned to digit ${best.template.digit}'s hidden states (Viterbi path):"),
              alignmentRows(observations, best.statePath, best.template.chain),

              <.div(^.cls := "hdw-section-title", "P(digit | your strokes), ranked:"),
              rankingBars(results)
            )
        )
      ),

      <.div(^.cls := "hdw-legend",
        for t <- templates yield
          <.div(^.cls := "hdw-legend-item",
            legendIcon(t.chain),
            t.digit.toString
          )
      )
    )
}
