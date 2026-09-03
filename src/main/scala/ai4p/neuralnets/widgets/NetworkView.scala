package ai4p.neuralnets.widgets

import com.wbillingsley.veautiful.html.{SVG, Styling, DSvgContent, ^}

import ai4p.{*, given}

/**
 * A small, reusable renderer for feedforward-network diagrams: nodes arranged in layers,
 * with edges drawn between adjacent layers. It's a pure function of the data you hand it
 * (no internal state of its own), so any widget can build a `Diagram` each render and get
 * a consistent look - wiring a network up is just describing its nodes and edges.
 */
object NetworkView {

  val styling = Styling(
    """|background: #fafafa;
       |border: 1px solid #eee;
       |border-radius: 4px;
       |""".stripMargin
  ).modifiedBy(
    " .nv-node-label" -> "font-family: monospace; font-size: 10px; fill: #333; pointer-events: none;",
    " .nv-layer-label" -> "font-family: 'Lato', sans-serif; font-size: 12px; fill: #777; pointer-events: none;"
  ).register()

  /** One neuron in a layer. `value` (if known) shades the node's fill; `label` is drawn beneath it. */
  case class Node(label: String = "", value: Option[Double] = None)

  /** A connection between node `fromIndex` of one layer and node `toIndex` of the next layer, carrying a weight for styling. */
  case class Edge(fromIndex: Int, toIndex: Int, weight: Double)

  /** The edges leaving one layer, i.e. `edgesFrom(i)` connects `layers(i)` to `layers(i+1)`. */
  case class Diagram(
    layers: Vector[Vector[Node]],
    edgesFrom: Vector[Vector[Edge]] = Vector.empty,
    width: Int = 480,
    height: Int = 260,
    nodeRadius: Double = 14,
    layerLabels: Vector[String] = Vector.empty
  ) {
    private val pad = nodeRadius + 10
    private val topPad = if layerLabels.isEmpty then pad else pad + 14
    // Reserve extra room at the bottom if any node has a label, since those are drawn just below it.
    private val bottomPad = pad + (if layers.exists(_.exists(_.label.nonEmpty)) then 16 else 0)

    private val layerX: Vector[Double] =
      if layers.size <= 1 then Vector(width / 2.0)
      else Vector.tabulate(layers.size)(i => pad + i * (width - 2 * pad) / (layers.size - 1).toDouble)

    private def nodeY(layerSize: Int, index: Int): Double =
      if layerSize <= 1 then (topPad + height - bottomPad) / 2.0
      else topPad + index * (height - topPad - bottomPad) / (layerSize - 1).toDouble

    def positionOf(layerIndex: Int, nodeIndex: Int): (Double, Double) =
      (layerX(layerIndex), nodeY(layers(layerIndex).size, nodeIndex))
  }

  /** Diverging blue/orange colour for a signed, roughly [-1,1]-scaled quantity (a weight or activation). */
  def divergingColor(v: Double): String =
    val clamped = math.max(-1.0, math.min(1.0, v))
    if clamped >= 0 then s"rgba(37, 99, 235, ${0.15 + 0.75 * clamped})"   // blue, stronger with size
    else s"rgba(234, 88, 12, ${0.15 + 0.75 * -clamped})"                 // orange, stronger with size

  private def edgeSvg(x1: Double, y1: Double, x2: Double, y2: Double, weight: Double): DSvgContent =
    val strokeWidth = 0.6 + 3.2 * math.min(1.0, math.abs(weight))
    SVG.line(
      ^.attr("x1") := x1, ^.attr("y1") := y1, ^.attr("x2") := x2, ^.attr("y2") := y2,
      ^.attr("stroke") := divergingColor(weight), ^.attr("stroke-width") := strokeWidth
    )

  private def nodeSvg(cx: Double, cy: Double, r: Double, node: Node): DSvgContent =
    val fill = node.value match
      case Some(v) => divergingColor(v)
      case None    => "white"
    SVG.g(
      SVG.circle(
        ^.attr("cx") := cx, ^.attr("cy") := cy, ^.attr("r") := r,
        ^.attr("fill") := fill, ^.attr("stroke") := "#555", ^.attr("stroke-width") := "1.2"
      ),
      if node.label.nonEmpty then
        SVG.text(^.cls := "nv-node-label", ^.attr("x") := cx, ^.attr("y") := cy + r + 12,
          ^.attr("text-anchor") := "middle", node.label)
      else SVG.g()
    )

  /** Renders a full standalone `<svg>` diagram for the given network description. */
  def render(d: Diagram): DSvgContent =
    SVG.svg(^.cls := styling.className, ^.attr("width") := d.width, ^.attr("height") := d.height,

      // Edges first, so nodes draw on top of them
      SVG.g(
        for
          (layerEdges, li) <- d.edgesFrom.zipWithIndex
          e <- layerEdges
        yield
          val (x1, y1) = d.positionOf(li, e.fromIndex)
          val (x2, y2) = d.positionOf(li + 1, e.toIndex)
          edgeSvg(x1, y1, x2, y2, e.weight)
      ),

      // Nodes
      SVG.g(
        for
          (layer, li) <- d.layers.zipWithIndex
          (node, ni) <- layer.zipWithIndex
        yield
          val (cx, cy) = d.positionOf(li, ni)
          nodeSvg(cx, cy, d.nodeRadius, node)
      ),

      // Optional per-layer captions along the top. The first/last labels are anchored to the
      // canvas edges (rather than centred on the node column) so long text doesn't clip off
      // the left or right of the svg.
      if d.layerLabels.isEmpty then SVG.g()
      else SVG.g(
        for (label, li) <- d.layerLabels.zipWithIndex if li < d.layers.size yield
          val (x, _) = d.positionOf(li, 0)
          val isFirst = li == 0
          val isLast = li == d.layers.size - 1
          val anchor = if isFirst then "start" else if isLast then "end" else "middle"
          val tx = if isFirst then 2.0 else if isLast then d.width - 2.0 else x
          SVG.text(^.cls := "nv-layer-label", ^.attr("x") := tx, ^.attr("y") := 14,
            ^.attr("text-anchor") := anchor, label)
      )
    )

  /** Convenience: a fully-connected `Diagram` between layers of the given sizes, with a weight
    * function `w(layerIndex, fromIndex, toIndex)`. `values(layerIndex, index)` optionally shades nodes. */
  def denseDiagram(
    layerSizes: Vector[Int],
    weight: (Int, Int, Int) => Double,
    value: (Int, Int) => Option[Double] = (_, _) => None,
    label: (Int, Int) => String = (_, _) => "",
    layerLabels: Vector[String] = Vector.empty,
    width: Int = 480,
    height: Int = 260,
    nodeRadius: Double = 14
  ): Diagram =
    val layers = layerSizes.zipWithIndex.map { (size, li) =>
      Vector.tabulate(size)(ni => Node(label(li, ni), value(li, ni)))
    }
    val edgesFrom = (0 until layerSizes.size - 1).map { li =>
      for
        from <- 0 until layerSizes(li)
        to <- 0 until layerSizes(li + 1)
      yield Edge(from, to, weight(li, from, to))
    }.map(_.toVector).toVector
    Diagram(layers, edgesFrom, width, height, nodeRadius, layerLabels)
}
