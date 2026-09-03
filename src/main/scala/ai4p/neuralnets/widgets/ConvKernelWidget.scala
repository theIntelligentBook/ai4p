package ai4p.neuralnets.widgets

import com.wbillingsley.veautiful.html.{<, SVG, Styling, DHtmlComponent, DSvgContent, ^}
import org.scalajs.dom

import ai4p.{*, given}


/**
 * A simple, mechanism-only (not trained) illustration of convolution: a fixed vertical-edge kernel
 * slides across a small input image one position at a time, and each step fills in one cell of the
 * output feature map - so you can see a convolution as "the same small set of weights, re-applied
 * at every position" rather than a fully-connected mess of one-off connections.
 */
object ConvKernelWidget {
  val inputSize = 8
  val kernelSize = 3
  val outSize = inputSize - kernelSize + 1
  val cell = 22

  // A darker region on the left, a lighter one on the right - one clean vertical edge - with
  // independent per-pixel noise on top, so it reads as a dappled, naturalistic grayscale texture
  // (like mottled light on a wall) rather than a flat block of colour. Fixed-seed, so it's the
  // same "photo" every time rather than re-rolling on every reload.
  private val noiseRnd = new scala.util.Random(42L)
  private val noise: Vector[Vector[Double]] = Vector.fill(inputSize, inputSize)((noiseRnd.nextDouble() - 0.5) * 0.3)

  val input: Vector[Vector[Double]] = Vector.tabulate(inputSize, inputSize) { (r, c) =>
    val base = if c < 4 then 0.25 else 0.75
    math.max(0.0, math.min(1.0, base + noise(r)(c)))
  }

  /** Maps a grayscale pixel value in [0,1] to an actual grey fill colour. */
  def grayscale(v: Double): String =
    val g = (math.max(0.0, math.min(1.0, v)) * 255).round
    s"rgb($g,$g,$g)"

  /** A classic vertical-edge detector: positive on a rising (dark-to-light) edge, negative on a falling one. */
  val kernel: Vector[Vector[Double]] = Vector(
    Vector(1.0, 0.0, -1.0),
    Vector(1.0, 0.0, -1.0),
    Vector(1.0, 0.0, -1.0)
  )

  val positions: Vector[(Int, Int)] = (for r <- 0 until outSize; c <- 0 until outSize yield (r, c)).toVector

  def convAt(r: Int, c: Int): Double =
    (for kr <- 0 until kernelSize; kc <- 0 until kernelSize yield kernel(kr)(kc) * input(r + kr)(c + kc)).sum

  val styling = Styling(
    """|display: inline-block;
       |font-family: 'Lato', sans-serif;
       |""".stripMargin
  ).modifiedBy(
    " .ck-row" -> "display: flex; gap: 28px; align-items: flex-start; flex-wrap: wrap; margin: 10px 0;",
    " .ck-col" -> "display: flex; flex-direction: column; align-items: center; gap: 4px;",
    " .ck-title" -> "font-size: 0.8rem; font-weight: bold; color: #555;",
    " .ck-controls" -> "margin-top: 8px; display: flex; gap: 8px; align-items: center;",
    " .ck-info" -> "font-family: monospace; font-size: 0.85rem; color: #444; margin-top: 6px;"
  ).register()

  def grid(size: Int, cellPx: Int, fillOf: (Int, Int) => String, labelOf: (Int, Int) => String = (_, _) => "",
           highlight: Option[(Int, Int, Int)] = None): DSvgContent =
    val px = size * cellPx
    SVG.svg(^.attr("width") := px, ^.attr("height") := px,
      SVG.g(
        for r <- 0 until size; c <- 0 until size yield
          SVG.g(
            SVG.rect(
              ^.attr("x") := c * cellPx, ^.attr("y") := r * cellPx, ^.attr("width") := cellPx, ^.attr("height") := cellPx,
              ^.attr("fill") := fillOf(r, c), ^.attr("stroke") := "#cbd5e1"
            ),
            if labelOf(r, c).nonEmpty then
              SVG.text(^.attr("x") := c * cellPx + cellPx / 2, ^.attr("y") := r * cellPx + cellPx / 2 + 4,
                ^.attr("text-anchor") := "middle", ^.attr("font-size") := "11", ^.attr("fill") := "#333", labelOf(r, c))
            else SVG.g()
          )
      ),
      highlight.map { (hr, hc, span) =>
        SVG.rect(^.attr("x") := hc * cellPx, ^.attr("y") := hr * cellPx,
          ^.attr("width") := span * cellPx, ^.attr("height") := span * cellPx,
          ^.attr("fill") := "none", ^.attr("stroke") := "#ef4444", ^.attr("stroke-width") := "3")
      }.getOrElse(SVG.g())
    )
}

case class ConvKernelWidget() extends DHtmlComponent {
  import ConvKernelWidget._

  private var doneCount: Int = 0
  private var autoRunning = false
  private var timerId: Option[Int] = None

  private def stepOnce(): Unit =
    if doneCount < positions.size then doneCount += 1
    rerender()

  private def reset(): Unit =
    stopAuto()
    doneCount = 0
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
      timerId = Some(dom.window.setInterval(() => {
        if doneCount < positions.size then doneCount += 1 else stopAuto()
        rerender()
      }, 120))
      rerender()

  override def afterDetach(): Unit = stopAuto()

  private def valueColor(v: Double): String = NetworkView.divergingColor(v / 2.0)

  override protected def render =
    val highlight =
      if doneCount < positions.size then
        val (r, c) = positions(doneCount)
        Some((r, c, kernelSize))
      else None

    <.div(^.cls := styling.className,
      <.div(^.cls := "ck-row",
        <.div(^.cls := "ck-col",
          <.div(^.cls := "ck-title", "Input"),
          grid(inputSize, cell, (r, c) => grayscale(input(r)(c)), highlight = highlight)
        ),
        <.div(^.cls := "ck-col",
          <.div(^.cls := "ck-title", "Kernel (fixed)"),
          grid(kernelSize, cell, (r, c) => valueColor(kernel(r)(c)), (r, c) => kernel(r)(c).round.toString)
        ),
        <.div(^.cls := "ck-col",
          <.div(^.cls := "ck-title", "Output feature map"),
          grid(outSize, cell, (r, c) =>
            val idx = r * outSize + c
            if idx < doneCount then valueColor(convAt(r, c)) else "#f1f5f9"
          )
        )
      ),

      <.div(^.cls := "ck-info", s"cells filled: $doneCount / ${positions.size}"),

      <.div(^.cls := "ck-controls",
        <.button(^.cls := "btn btn-outline-primary btn-sm", ^.onClick --> stepOnce(), "Step"),
        <.button(^.cls := (if autoRunning then "btn btn-warning btn-sm" else "btn btn-outline-success btn-sm"),
          ^.onClick --> toggleAuto(), if autoRunning then "Stop" else "Auto"),
        <.button(^.cls := "btn btn-outline-secondary btn-sm", ^.onClick --> reset(), "Reset")
      )
    )
}
