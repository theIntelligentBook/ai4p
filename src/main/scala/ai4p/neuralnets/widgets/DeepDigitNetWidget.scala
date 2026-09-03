package ai4p.neuralnets.widgets

import com.wbillingsley.veautiful.html.{<, Styling, DHtmlComponent, VHtmlContent, ^}
import org.scalajs.dom

import ai4p.{*, given}

object DeepDigitNetWidget {
  import TinyDigitNetModel.gridSize

  val cell = 14
  val gap = 2

  val styling = Styling(
    """|display: inline-block;
       |font-family: 'Lato', sans-serif;
       |""".stripMargin
  ).modifiedBy(
    " .ddn-columns" -> "display: flex; gap: 20px; flex-wrap: wrap; align-items: flex-start; margin: 10px 0;",
    " .ddn-left-col" -> "flex: 0 0 340px;",
    " .ddn-right-col" -> "flex: 0 0 300px;",
    " .ddn-row" -> "display: flex; gap: 10px; align-items: flex-end; flex-wrap: wrap;",
    " .ddn-tile" -> "display: flex; flex-direction: column; align-items: center; gap: 4px; border: 2px solid transparent; border-radius: 6px; padding: 3px;",
    " .ddn-tile.ddn-spotlight" -> "border-color: #f59e0b; background: #fffbeb;",
    " .ddn-tile-label" -> "font-family: monospace; font-weight: bold; color: #1d4ed8; font-size: 0.8rem;",
    " .ddn-section-title" -> "font-size: 0.8rem; font-weight: bold; color: #555; margin-top: 10px;",
    " .ddn-controls" -> "margin-top: 8px; display: flex; gap: 8px; align-items: center; flex-wrap: wrap;",
    " .ddn-info" -> "font-family: monospace; font-size: 0.85rem; color: #444; margin-top: 6px;",
    " .ddn-status" -> "font-size: 0.9rem; color: #333; margin-top: 8px; padding: 6px 10px; background: #f8fafc; border-radius: 4px; border-left: 3px solid #f59e0b;",
    " .ddn-note" -> "font-size: 0.8rem; margin-top: 4px;",
    " .ddn-note.ddn-seen" -> "color: #166534;",
    " .ddn-note.ddn-unseen" -> "color: #991b1b;",
    " .ddn-dpad" -> "display: grid; grid-template-columns: repeat(3, 28px); grid-template-rows: repeat(3, 28px); gap: 2px;",
    " .ddn-dpad button" -> "padding: 0;",
    " .ddn-bar-row" -> "display: flex; align-items: center; gap: 6px; margin: 2px 0; font-size: 0.85rem;",
    " .ddn-bar-row.ddn-top" -> "font-weight: bold; color: #166534;",
    " .ddn-bar-label" -> "width: 16px; text-align: center; font-weight: bold; font-family: monospace;",
    " .ddn-bar-track" -> "flex: 1; height: 10px; background: #eee; border-radius: 3px; overflow: hidden; max-width: 160px;",
    " .ddn-bar-fill" -> "height: 100%; background: #93c5fd;",
    " .ddn-bar-row.ddn-top .ddn-bar-fill" -> "background: #16a34a;",
    " .ddn-bar-pct" -> "width: 34px; font-size: 0.75rem; color: #666;"
  ).register()

  def gridSvg(pixels: Vector[Int], editable: Boolean = false, onToggle: Int => Unit = _ => ()) =
    PixelGrid.svg(pixels, gridSize, cell, gap, editable, onToggle)
}

/**
 * A deeper cousin of [[TinyDigitNetWidget]]: 25 -> 10 -> 6 -> 3, trained not just on the three
 * base digit shapes but on each one nudged a pixel in every direction - so recognising a digit
 * means learning the *relationship* between its strokes, not memorising one fixed arrangement of
 * pixels. The same feed-forward/adjust-weights walkthrough as the shallow network shows the extra
 * layer of composition happening, and a separate "nudge tester" lets you check whether the learned
 * tolerance extends to shifts the network was never actually trained on.
 */
case class DeepDigitNetWidget() extends DHtmlComponent {
  import DeepDigitNetWidget._
  import TinyDigitNetModel.{gridSize, nPixels, argmax, templates}
  import DeepDigitNetModel._

  private var net = new DeepDigitNet()
  private var autoRunning = false
  private var timerId: Option[Int] = None

  // Walkthrough state, same shape as TinyDigitNetWidget's but over the larger (shifted) training set.
  private var currentIdx: Int = 0
  private var awaitingUpdate: Boolean = false
  private var lastA1: Vector[Double] = Vector.fill(nHidden1)(0.0)
  private var lastA2: Vector[Double] = Vector.fill(nHidden2)(0.0)
  private var lastA3: Vector[Double] = Vector.fill(nOut)(0.0)

  // Nudge-tester state: an independent digit + offset, used to seed an editable pixel grid you
  // can then hand-correct - useful since shifting a full-size digit by 2 pixels pushes much of it
  // off the edge of the grid, so a straight shift alone doesn't leave enough shape to recognise.
  private var testDigitIdx: Int = 0
  private var testDx: Int = 0
  private var testDy: Int = 0
  private var testPixels: Array[Int] = shift(templates(0).pixels, 0, 0).toArray

  private def feedForward(): Unit =
    val f = net.forward(trainingSet(currentIdx).pixels)
    lastA1 = f.a1; lastA2 = f.a2; lastA3 = f.a3
    awaitingUpdate = true
    rerender()

  private def adjustWeights(): Unit =
    net.stepOne(currentIdx)
    awaitingUpdate = false
    currentIdx = (currentIdx + 1) % trainingSet.size
    rerender()

  private def walkthroughStep(): Unit = if awaitingUpdate then adjustWeights() else feedForward()

  private def trainBurst(n: Int = 400): Unit =
    for _ <- 1 to n do net.step()
    awaitingUpdate = false
    rerender()

  private def resetNet(): Unit =
    stopAuto()
    net = new DeepDigitNet(seed = (math.random() * 1e9).toLong)
    currentIdx = 0
    awaitingUpdate = false
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
      timerId = Some(dom.window.setInterval(() => walkthroughStep(), 700))
      rerender()

  override def afterDetach(): Unit = stopAuto()

  private def presetPixels: Vector[Int] = shift(templates(testDigitIdx).pixels, testDx, testDy)

  private def loadPreset(): Unit = testPixels = presetPixels.toArray

  private def selectTestDigit(i: Int): Unit =
    testDigitIdx = i
    loadPreset()
    rerender()

  private def nudge(ddx: Int, ddy: Int): Unit =
    testDx = math.max(-2, math.min(2, testDx + ddx))
    testDy = math.max(-2, math.min(2, testDy + ddy))
    loadPreset()
    rerender()

  private def recenter(): Unit =
    testDx = 0; testDy = 0
    loadPreset()
    rerender()

  private def toggleTestPixel(i: Int): Unit =
    testPixels(i) = 1 - testPixels(i)
    rerender()

  private def clearTestPixels(): Unit =
    testPixels = Array.fill(nPixels)(0)
    rerender()

  private def predictionBars(probs: Vector[Double], bestIdx: Int): VHtmlContent =
    <.div(
      for (t, i) <- templates.zipWithIndex yield
        <.div(^.cls := s"ddn-bar-row${if i == bestIdx then " ddn-top" else ""}",
          <.span(^.cls := "ddn-bar-label", t.name),
          <.div(^.cls := "ddn-bar-track", <.div(^.cls := "ddn-bar-fill", ^.style := s"width: ${(probs(i) * 100).round}%;")),
          <.span(^.cls := "ddn-bar-pct", s"${(probs(i) * 100).round}%")
        )
    )

  override protected def render =
    val ex = trainingSet(currentIdx)
    val scale1 = math.max(1e-6, net.w1.flatten.map(math.abs).max)
    val scale2 = math.max(1e-6, net.w2.flatten.map(math.abs).max)
    val scale3 = math.max(1e-6, net.w3.flatten.map(math.abs).max)

    val diagram = NetworkView.denseDiagram(
      layerSizes = Vector(nPixels, nHidden1, nHidden2, nOut),
      weight = (li, from, to) => li match
        case 0 => net.w1(from)(to) / scale1
        case 1 => net.w2(from)(to) / scale2
        case _ => net.w3(from)(to) / scale3,
      value = (li, ni) => li match
        case 0 => Some(ex.pixels(ni).toDouble)
        case 1 => if awaitingUpdate then Some(lastA1(ni)) else None
        case 2 => if awaitingUpdate then Some(lastA2(ni)) else None
        case _ => if awaitingUpdate then Some(lastA3(ni)) else None,
      label = (li, ni) => if li == 3 && awaitingUpdate then f"${lastA3(ni)}%.2f" else "",
      layerLabels = Vector(s"$nPixels pixels", s"$nHidden1 hidden", s"$nHidden2 hidden", "3 digits"),
      width = 720, height = 640, nodeRadius = 10
    )

    val offsetStr = s"(Δx=${if ex.dx >= 0 then "+" else ""}${ex.dx}, Δy=${if ex.dy >= 0 then "+" else ""}${ex.dy})"
    val statusText =
      if awaitingUpdate then
        val guessIdx = argmax(lastA3)
        val pct = (lastA3(guessIdx) * 100).round
        s"② Fed '${ex.name}' (${ex.variant}) $offsetStr forward → guessed '${templates(guessIdx).name}' ($pct%). Click Adjust weights to correct it."
      else
        s"① Loaded '${ex.name}' (${ex.variant}) $offsetStr. Click Feed forward to see the current prediction."

    val testForward = net.forward(testPixels.toVector)
    val testBest = argmax(testForward.a3)
    val isEdited = testPixels.toVector != presetPixels
    val seenInTraining = !isEdited && trainingOffsets.contains((testDx, testDy))

    <.div(^.cls := styling.className,
      <.div(^.cls := "ddn-section-title", "Training examples (each digit drawn more than one way; amber = currently loaded)"),
      <.div(^.cls := "ddn-row",
        (for s <- shapeLibrary yield
          <.div(^.cls := "ddn-tile", gridSvg(s.pixels), <.div(^.cls := "ddn-tile-label", s"${s.digitName} (${s.variant})"))
        ) :+ <.div(^.cls := "ddn-tile ddn-spotlight", gridSvg(ex.pixels), <.div(^.cls := "ddn-tile-label", s"${ex.name} (${ex.variant}) $offsetStr"))
      ),

      <.div(^.cls := "ddn-columns",
        <.div(^.cls := "ddn-left-col",
          <.div(^.cls := "ddn-status", statusText),

          <.div(^.cls := "ddn-controls",
            <.button(^.cls := "btn btn-outline-primary btn-sm", ^.onClick --> walkthroughStep(),
              if awaitingUpdate then "② Adjust weights" else "① Feed forward"),
            <.button(^.cls := (if autoRunning then "btn btn-warning btn-sm" else "btn btn-outline-success btn-sm"),
              ^.onClick --> toggleAuto(), if autoRunning then "Stop" else "Auto"),
            <.button(^.cls := "btn btn-outline-primary btn-sm", ^.onClick --> trainBurst(), "Train 400 (fast)"),
            <.button(^.cls := "btn btn-outline-secondary btn-sm", ^.onClick --> resetNet(), "Reset weights")
          ),

          <.div(^.cls := "ddn-info", f"epoch ${net.epoch}   loss ${net.loss()}%.3f   ${net.correctCount()}/${trainingSet.size} correct")
        ),

        <.div(^.cls := "ddn-right-col",
          <.div(^.cls := "ddn-section-title", "Test: recognise a digit that's off-centre"),
          <.div(^.cls := "ddn-controls",
            for (t, i) <- templates.zipWithIndex yield
              <.button(^.cls := (if i == testDigitIdx then "btn btn-primary btn-sm" else "btn btn-outline-primary btn-sm"),
                ^.onClick --> selectTestDigit(i), t.name)
          ),

          <.div(^.cls := "ddn-row",
            gridSvg(testPixels.toVector, editable = true, onToggle = toggleTestPixel),
            <.div(^.cls := "ddn-dpad",
              <.span(), <.button(^.cls := "btn btn-outline-secondary btn-sm", ^.onClick --> nudge(0, -1), "▲"), <.span(),
              <.button(^.cls := "btn btn-outline-secondary btn-sm", ^.onClick --> nudge(-1, 0), "◀"),
              <.button(^.cls := "btn btn-outline-secondary btn-sm", ^.onClick --> recenter(), "●"),
              <.button(^.cls := "btn btn-outline-secondary btn-sm", ^.onClick --> nudge(1, 0), "▶"),
              <.span(), <.button(^.cls := "btn btn-outline-secondary btn-sm", ^.onClick --> nudge(0, 1), "▼"), <.span()
            )
          ),
          <.div(^.cls := "ddn-controls",
            <.button(^.cls := "btn btn-outline-secondary btn-sm", ^.onClick --> clearTestPixels(), "Clear"),
            <.button(^.cls := "btn btn-outline-secondary btn-sm", ^.onClick --> { loadPreset(); rerender() }, "Reset to shifted preset")
          ),

          <.div(^.cls := "ddn-note",
            "Click pixels to draw or correct the shape by hand - handy since a big shift pushes most of a full-size digit off the grid."),
          <.div(^.cls := s"ddn-note ${if seenInTraining then "ddn-seen" else "ddn-unseen"}",
            if isEdited then "hand-edited - not a plain shift, so this is a genuine test of what shape it learned"
            else s"offset (Δx=${if testDx >= 0 then "+" else ""}$testDx, Δy=${if testDy >= 0 then "+" else ""}$testDy): " +
              (if seenInTraining then "seen during training" else "never seen during training - this is generalisation")),

          <.div(^.cls := "ddn-section-title", s"Network's guess: ${templates(testBest).name}"),
          predictionBars(testForward.a3, testBest)
        )
      ),

      <.div(^.cls := "ddn-section-title", "Network (fill = activation, edge colour = weight)"),
      NetworkView.render(diagram)
    )
}
