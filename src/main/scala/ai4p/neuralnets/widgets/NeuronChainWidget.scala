package ai4p.neuralnets.widgets

import com.wbillingsley.veautiful.html.{<, Styling, DHtmlComponent, ^}
import org.scalajs.dom

import ai4p.{*, given}

/**
 * A chain of `reps` "linear, then activation" blocks - `z = w·prev + b`, `a = act(z)` - repeated,
 * with no width anywhere, so you can see exactly what stacking does to a signal before anything as
 * complicated as a full network is in the picture.
 *
 * With `expandLinear = false` (the default) each block is drawn as a single fused neuron - the
 * natural view for a *linear* activation, since composing linear functions gives you a linear
 * function no matter how many you stack (the whole reason a nonlinear activation is needed at
 * all). With `expandLinear = true` the linear step and the activation are drawn as *separate*
 * nodes in the diagram instead, since the parameters (`w`, `b`) live only on the linear step - the
 * activation function itself has none. The two are mathematically identical either way; this only
 * changes how it's drawn.
 */
object NeuronChainWidget {

  def linear(z: Double): Double = z
  def step(z: Double): Double = if z >= 0.0 then 1.0 else 0.0
  def sigmoid(z: Double): Double = 1.0 / (1.0 + math.exp(-z))
  def relu(z: Double): Double = math.max(0.0, z)

  /** SiLU / Swish: `z * sigmoid(z)` - like a smoothed-out ReLU that dips slightly negative before
    * flattening to 0. The nonlinearity used inside SwiGLU. */
  def silu(z: Double): Double = z * sigmoid(z)

  private val subscripts = Vector("₁", "₂", "₃", "₄", "₅")

  val styling = Styling(
    """|display: inline-block;
       |font-family: 'Lato', sans-serif;
       |""".stripMargin
  ).modifiedBy(
    " .nc-controls" -> "margin-top: 8px; display: flex; gap: 10px; align-items: center; flex-wrap: wrap;",
    " .nc-weight" -> "display: flex; align-items: center; gap: 6px;",
    " .nc-info" -> "font-family: monospace; font-size: 0.85rem; color: #444; margin-top: 6px;",
    " .nc-label" -> "font-size: 0.85rem; color: #666;",
    " .nc-row" -> "display: flex; gap: 24px; flex-wrap: wrap; align-items: center;",
    " input[type=range]" -> "width: 110px;"
  ).register()
}

/** @param activations named activation functions to choose between - a button per entry appears
  *                     only if more than one is given; with just one, it's used silently.
  *  @param reps        how many "linear, then activation" blocks to chain.
  *  @param expandLinear when true, draws the linear step and the activation as separate nodes.
  *  @param showBias    when true, also shows a bias slider for each linear step (off by default). */
case class NeuronChainWidget(
  activations: Vector[(String, Double => Double)],
  reps: Int = 3,
  expandLinear: Boolean = false,
  xMin: Double = -5.0,
  xMax: Double = 5.0,
  yMin: Double = -5.0,
  yMax: Double = 5.0,
  wMin: Double = -2.0,
  wMax: Double = 2.0,
  showBias: Boolean = false,
  bMin: Double = -2.0,
  bMax: Double = 2.0
) extends DHtmlComponent {
  import NeuronChainWidget._

  private val frame = PlotFrame(xMin = xMin, xMax = xMax, yMin = yMin, yMax = yMax, w = 480, h = 320)

  private var activeIdx: Int = 0
  private val w: Array[Double] = Array.fill(reps)(1.0)
  private val b: Array[Double] = Array.fill(reps)(0.0)

  private def act(z: Double): Double = activations(activeIdx)._2(z)

  private def setW(i: Int, v: Double): Unit = { w(i) = math.max(wMin, math.min(wMax, v)); rerender() }
  private def setB(i: Int, v: Double): Unit = { b(i) = math.max(bMin, math.min(bMax, v)); rerender() }
  private def selectActivation(i: Int): Unit = { activeIdx = i; rerender() }

  private def reset(): Unit =
    for i <- 0 until reps do { w(i) = 1.0; b(i) = 0.0 }
    rerender()

  private def output(x: Double): Double =
    var current = x
    for i <- 0 until reps do current = act(w(i) * current + b(i))
    current

  /** Diagram with a fused node per block: layers are `x, block_1, ..., block_reps`. */
  private def fusedDiagram(name: String, scale: Double): NetworkView.Diagram =
    NetworkView.denseDiagram(
      layerSizes = Vector.fill(reps + 1)(1),
      weight = (li, _, _) => w(li) / scale,
      label = (li, _) => if li == 0 then "x" else name,
      layerLabels = Vector("x") ++ Vector.fill(reps - 1)("") ++ Vector("y"),
      width = math.max(240, 90 * (reps + 1)), height = 160, nodeRadius = 16
    )

  /** Diagram with the linear step and the activation drawn as separate nodes: layers are
    * `x, linear_1, act_1, linear_2, act_2, ...` - the learnable weight sits only on the edge going
    * *into* a linear node; the edge from a linear node into its activation is a fixed pass-through. */
  private def expandedDiagram(name: String, scale: Double): NetworkView.Diagram =
    NetworkView.denseDiagram(
      layerSizes = Vector(1) ++ Vector.fill(reps * 2)(1),
      weight = (li, _, _) => if li % 2 == 0 then w(li / 2) / scale else 1.0,
      label = (li, _) =>
        if li == 0 then "x"
        else if li % 2 == 1 then "linear"
        else name,
      layerLabels = Vector("x") ++ Vector.fill(reps * 2 - 1)("") ++ Vector("y"),
      width = math.max(360, 90 * (reps * 2 + 1)), height = 170, nodeRadius = 14
    )

  private def diagram: NetworkView.Diagram =
    val name = activations(activeIdx)._1
    val scale = math.max(1.0, w.map(math.abs).max)
    if expandLinear then expandedDiagram(name, scale) else fusedDiagram(name, scale)

  private def slider(label: String, value: Double, min: Double, max: Double, onChange: Double => Unit) =
    <.div(^.cls := "nc-weight",
      <.span(^.cls := "nc-label", label),
      <("input")(
        ^.attr("type") := "range", ^.attr("min") := min.toString, ^.attr("max") := max.toString, ^.attr("step") := "0.1",
        ^.attr("value") := value.toString,
        ^.on("input") ==> { (e: dom.Event) => onChange(e.target.asInstanceOf[dom.html.Input].value.toDouble) }
      ),
      <.span(^.cls := "nc-label", f"$value%.1f")
    )

  private def formula: String =
    val wrapOneBlock = (inner: String, i: Int) =>
      if showBias then s"act(w${subscripts(i)}·$inner+b${subscripts(i)})"
      else s"act(w${subscripts(i)}·$inner)"
    "y = " + (0 until reps).foldLeft("x")(wrapOneBlock)

  override protected def render =
    <.div(^.cls := styling.className,
      <.div(^.cls := "nc-row",
        NetworkView.render(diagram),
        frame.svg(frame.axes, frame.curve(output, stroke = "#7c3aed", strokeWidth = 2.5))
      ),

      <.div(^.cls := "nc-info", s"$formula,   activation = ${activations(activeIdx)._1}"),

      <.div(^.cls := "nc-controls",
        for i <- 0 until reps yield slider(s"w${subscripts(i)} =", w(i), wMin, wMax, v => setW(i, v)),
        <.button(^.cls := "btn btn-outline-secondary btn-sm", ^.onClick --> reset(), "Reset")
      ),

      if showBias then
        <.div(^.cls := "nc-controls",
          for i <- 0 until reps yield slider(s"b${subscripts(i)} =", b(i), bMin, bMax, v => setB(i, v))
        )
      else <.span(),

      if activations.size > 1 then
        <.div(^.cls := "nc-controls",
          for (pair, i) <- activations.zipWithIndex yield
            <.button(^.cls := (if i == activeIdx then "btn btn-primary btn-sm" else "btn btn-outline-primary btn-sm"),
              ^.onClick --> selectActivation(i), pair._1)
        )
      else <.span()
    )
}
