package ai4p.neuralnets.widgets

import com.wbillingsley.veautiful.html.{<, Styling, DHtmlComponent, VHtmlContent, ^}
import org.scalajs.dom

import ai4p.{*, given}

object TinyDigitNetWidget {
  import TinyDigitNetModel._

  val cell = 16
  val gap = 2

  val styling = Styling(
    """|display: inline-block;
       |font-family: 'Lato', sans-serif;
       |""".stripMargin
  ).modifiedBy(
    " .tdn-columns" -> "display: flex; gap: 28px; flex-wrap: wrap; align-items: flex-start; margin: 10px 0;",
    " .tdn-left-col" -> "flex: 0 0 640px;",
    " .tdn-templates" -> "display: flex; gap: 10px;",
    " .tdn-template" -> "display: flex; flex-direction: column; align-items: center; gap: 4px; border: 2px solid transparent; border-radius: 6px; padding: 3px;",
    " .tdn-template.tdn-spotlight" -> "border-color: #f59e0b; background: #fffbeb;",
    " .tdn-template-label" -> "font-family: monospace; font-weight: bold; color: #1d4ed8;",
    " .tdn-section-title" -> "font-size: 0.8rem; font-weight: bold; color: #555; margin-top: 10px;",
    " .tdn-controls" -> "margin-top: 8px; display: flex; gap: 8px; align-items: center; flex-wrap: wrap;",
    " .tdn-info" -> "font-family: monospace; font-size: 0.85rem; color: #444; margin-top: 6px;",
    " .tdn-status" -> "font-size: 0.9rem; color: #333; margin-top: 8px; padding: 6px 10px; background: #f8fafc; border-radius: 4px; border-left: 3px solid #f59e0b;",
    " .tdn-bar-row" -> "display: flex; align-items: center; gap: 6px; margin: 2px 0; font-size: 0.85rem;",
    " .tdn-bar-row.tdn-top" -> "font-weight: bold; color: #166534;",
    " .tdn-bar-label" -> "width: 16px; text-align: center; font-weight: bold; font-family: monospace;",
    " .tdn-bar-track" -> "flex: 1; height: 10px; background: #eee; border-radius: 3px; overflow: hidden; max-width: 160px;",
    " .tdn-bar-fill" -> "height: 100%; background: #93c5fd;",
    " .tdn-bar-row.tdn-top .tdn-bar-fill" -> "background: #16a34a;",
    " .tdn-bar-pct" -> "width: 34px; font-size: 0.75rem; color: #666;"
  ).register()

  def gridSvg(pixels: Vector[Int], editable: Boolean, onToggle: Int => Unit = _ => ()) =
    PixelGrid.svg(pixels, gridSize, cell, gap, editable, onToggle)
}

/**
 * A tiny neural network learning to tell three pixelated "digits" apart, with backprop broken into
 * two visible half-steps: **Feed forward** loads one training digit in and shows what the network
 * currently predicts for it, then **Adjust weights** runs backprop for exactly that example and
 * shows the result - so each click is either "see the output" or "change the weights", never both
 * at once. A separate scratch grid lets you draw your own pattern to test what's been learned.
 */
case class TinyDigitNetWidget() extends DHtmlComponent {
  import TinyDigitNetWidget._
  import TinyDigitNetModel._

  private var net = new TinyDigitNet()
  private var customPixels: Array[Int] = Array.fill(nPixels)(0)
  private var autoRunning = false
  private var timerId: Option[Int] = None

  // Walkthrough state: which example is "loaded", and whether we've fed it forward yet (in which
  // case lastA1/lastA2 hold that forward pass, ready to be corrected by Adjust weights).
  private var currentExample: Int = 0
  private var awaitingUpdate: Boolean = false
  private var lastA1: Vector[Double] = Vector.fill(nHidden)(0.0)
  private var lastA2: Vector[Double] = Vector.fill(templates.size)(0.0)

  private def toggleCustomPixel(i: Int): Unit =
    customPixels(i) = 1 - customPixels(i)
    rerender()

  private def clearCustom(): Unit =
    customPixels = Array.fill(nPixels)(0)
    rerender()

  /** Half-step 1: run the current example through the (unchanged) network and show the result. */
  private def feedForward(): Unit =
    val f = net.forward(templates(currentExample).pixels)
    lastA1 = f.a1
    lastA2 = f.a2
    awaitingUpdate = true
    rerender()

  /** Half-step 2: correct the weights based on that forward pass's error, then load the next example. */
  private def adjustWeights(): Unit =
    net.stepOne(currentExample)
    awaitingUpdate = false
    currentExample = (currentExample + 1) % templates.size
    rerender()

  private def walkthroughStep(): Unit = if awaitingUpdate then adjustWeights() else feedForward()

  private def trainBurst(n: Int = 50): Unit =
    for _ <- 1 to n do net.step()
    awaitingUpdate = false
    rerender()

  private def resetNet(): Unit =
    stopAuto()
    net = new TinyDigitNet(seed = (math.random() * 1e9).toLong)
    currentExample = 0
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
      timerId = Some(dom.window.setInterval(() => walkthroughStep(), 900))
      rerender()

  override def afterDetach(): Unit = stopAuto()

  private def predictionBars(probs: Vector[Double], bestIdx: Int): VHtmlContent =
    <.div(
      for (t, i) <- templates.zipWithIndex yield
        <.div(^.cls := s"tdn-bar-row${if i == bestIdx then " tdn-top" else ""}",
          <.span(^.cls := "tdn-bar-label", t.name),
          <.div(^.cls := "tdn-bar-track", <.div(^.cls := "tdn-bar-fill", ^.style := s"width: ${(probs(i) * 100).round}%;")),
          <.span(^.cls := "tdn-bar-pct", s"${(probs(i) * 100).round}%")
        )
    )

  override protected def render =
    val loadedName = templates(currentExample).name
    val custom = net.forward(customPixels.toVector)
    val customBest = argmax(custom.a2)
    val scaleW1 = math.max(1e-6, net.w1.flatten.map(math.abs).max)
    val scaleW2 = math.max(1e-6, net.w2.flatten.map(math.abs).max)

    val diagram = NetworkView.denseDiagram(
      layerSizes = Vector(nPixels, nHidden, templates.size),
      weight = (li, from, to) => if li == 0 then net.w1(from)(to) / scaleW1 else net.w2(from)(to) / scaleW2,
      value = (li, ni) => li match
        case 0 => Some(templates(currentExample).pixels(ni).toDouble)
        case 1 => if awaitingUpdate then Some(lastA1(ni)) else None
        case _ => if awaitingUpdate then Some(lastA2(ni)) else None,
      label = (li, ni) => if li == 2 && awaitingUpdate then f"${lastA2(ni)}%.2f" else "",
      layerLabels = Vector(s"$nPixels pixels", s"$nHidden hidden", "3 digits"),
      width = 640, height = 640, nodeRadius = 10
    )

    val statusText =
      if awaitingUpdate then
        val guessIdx = argmax(lastA2)
        val pct = (lastA2(guessIdx) * 100).round
        s"② Fed '$loadedName' forward → the network guessed '${templates(guessIdx).name}' ($pct%). Click Adjust weights to correct it towards '$loadedName'."
      else
        s"① Loaded '$loadedName'. Click Feed forward to see what the network currently predicts for it."

    <.div(^.cls := styling.className,
      <.div(^.cls := "tdn-columns",
        <.div(^.cls := "tdn-left-col",
          <.div(^.cls := "tdn-section-title", "Training examples (highlighted = currently loaded)"),
          <.div(^.cls := "tdn-templates",
            for (t, i) <- templates.zipWithIndex yield
              <.div(^.cls := s"tdn-template${if i == currentExample then " tdn-spotlight" else ""}",
                gridSvg(t.pixels, editable = false), <.div(^.cls := "tdn-template-label", t.name))
          ),

          <.div(^.cls := "tdn-status", statusText),

          <.div(^.cls := "tdn-controls",
            <.button(^.cls := "btn btn-outline-primary btn-sm", ^.onClick --> walkthroughStep(),
              if awaitingUpdate then "② Adjust weights" else "① Feed forward"),
            <.button(^.cls := (if autoRunning then "btn btn-warning btn-sm" else "btn btn-outline-success btn-sm"),
              ^.onClick --> toggleAuto(), if autoRunning then "Stop" else "Auto"),
            <.button(^.cls := "btn btn-outline-primary btn-sm", ^.onClick --> trainBurst(), "Train 50 (fast)"),
            <.button(^.cls := "btn btn-outline-secondary btn-sm", ^.onClick --> resetNet(), "Reset weights")
          ),

          <.div(^.cls := "tdn-info", f"epoch ${net.epoch}   loss ${net.loss()}%.3f   ${net.correctCount()}/${templates.size} correct")
        ),

        <.div(
          <.div(^.cls := "tdn-section-title", "Draw a test digit"),
          gridSvg(customPixels.toVector, editable = true, onToggle = toggleCustomPixel),
          <.div(^.cls := "tdn-controls", <.button(^.cls := "btn btn-outline-secondary btn-sm", ^.onClick --> clearCustom(), "Clear")),

          <.div(^.cls := "tdn-section-title", s"Network's guess: ${templates(customBest).name}"),
          predictionBars(custom.a2, customBest)
        )
      ),

      <.div(^.cls := "tdn-section-title", "Network (fill = activation, edge colour = weight)"),
      NetworkView.render(diagram)
    )
}
