package ai4p.embeddings.widgets

import com.wbillingsley.veautiful.html.{<, SVG, Styling, DHtmlComponent, DSvgContent, ^}
import org.scalajs.dom
import ai4p.{*, given}
import ai4p.neuralnets.widgets.PlotFrame

/**
 * Trains [[TinyWord2Vec]] live, in the browser, on nothing but the text of Peter Rabbit - and
 * plots its 2-dimensional embeddings as it goes. Small enough to run in real time, but the corpus
 * is tiny: watch the points drift for a while and judge for yourself whether it's learned anything
 * you'd trust, before the next slide switches to vectors trained on a lot more text than this.
 */
object Word2VecWidget {
  import Word2VecModel._

  val styling = Styling(
    """|display: inline-block;
       |font-family: 'Lato', sans-serif;
       |""".stripMargin
  ).modifiedBy(
    " .w2v-controls" -> "margin-top: 8px; display: flex; align-items: center; gap: 8px; flex-wrap: wrap;",
    " .w2v-info" -> "font-family: monospace; font-size: 0.85rem; color: #444; margin-top: 6px;",
    " .w2v-vector" -> "font-family: monospace; font-size: 0.95rem; color: #1d4ed8; margin-top: 6px;",
    " .w2v-point" -> "cursor: pointer;"
  ).register()

  /** The story words worth labelling on the scatter - training still uses the full vocabulary
    * (stopwords included) as context, this is just what's legible to plot. */
  val labelled: Vector[String] = LineVectorModel.curatedSample
}

case class Word2VecWidget() extends DHtmlComponent {
  import Word2VecModel._
  import Word2VecWidget._

  private var net = new TinyWord2Vec()
  private var lastLoss: Double = Double.NaN
  private var running = false
  private var selected: Option[String] = labelled.headOption

  private def select(w: String): Unit =
    selected = Some(w)
    rerender()

  // A single epoch takes a few hundred ms - far longer than a frame - so training runs as a
  // self-rescheduling requestAnimationFrame loop (one epoch, then yield) rather than setInterval:
  // a fixed-period interval shorter than the work it's doing just queues up overdue callbacks
  // back-to-back, which was starving the Stop button's click event for seconds at a time.
  private def tick(t: Double): Unit =
    if running then
      lastLoss = net.trainEpoch()
      rerender()
      dom.window.requestAnimationFrame(tick)

  private def toggleAuto(): Unit =
    if running then
      running = false
      rerender()
    else
      running = true
      rerender()
      dom.window.requestAnimationFrame(tick)

  private def reset(): Unit =
    running = false
    net = new TinyWord2Vec(seed = (math.random() * 1e9).toLong)
    lastLoss = Double.NaN
    rerender()

  override def afterDetach(): Unit = running = false

  private def scatter: DSvgContent =
    val coords = labelled.map(w => w -> net.center(indexOf(w)))
    val furthest = coords.map((_, v) => math.max(math.abs(v(0)), math.abs(v(1)))).maxOption.getOrElse(1.0)
    val r = math.min(20.0, math.max(1.0, furthest * 1.25))
    val frame = PlotFrame(xMin = -r, xMax = r, yMin = -r, yMax = r, w = 480, h = 380)
    frame.svg(
      frame.axes,
      SVG.g(
        for (w, v) <- coords yield
          val isSelected = selected.contains(w)
          SVG.g(^.cls := "w2v-point", ^.onClick --> select(w),
            frame.dot(v(0), v(1), r = if isSelected then 6 else 4, fill = if isSelected then "#1d4ed8" else "#16a34a"),
            SVG.text(^.attr("x") := frame.toSvgX(v(0)) + 6, ^.attr("y") := frame.toSvgY(v(1)) - 6,
              ^.attr("font-size") := "11", ^.attr("font-family") := "monospace",
              ^.attr("font-weight") := (if isSelected then "bold" else "normal"),
              ^.attr("fill") := (if isSelected then "#1d4ed8" else "#000"), w)
          )
      )
    )

  private def vectorReadout =
    selected match
      case Some(w) =>
        val v = net.center(indexOf(w))
        <.div(^.cls := "w2v-vector", f"$w → (${v(0)}%.3f, ${v(1)}%.3f)")
      case None =>
        <.div(^.cls := "w2v-vector", "Click a word to see its vector.")

  override protected def render =
    <.div(^.cls := styling.className,
      scatter,
      vectorReadout,
      <.div(^.cls := "w2v-controls",
        <.button(^.cls := (if running then "btn btn-warning btn-sm" else "btn btn-outline-success btn-sm"),
          ^.onClick --> toggleAuto(), if running then "Stop" else "Train"),
        <.button(^.cls := "btn btn-outline-secondary btn-sm", ^.onClick --> reset(), "Reset")
      ),
      <.div(^.cls := "w2v-info",
        f"epoch ${net.epoch}" + (if lastLoss.isNaN then "" else f"   loss $lastLoss%.2f") +
        s"   ${vocab.size} words, ${pairs.size} training pairs"
      )
    )
}
