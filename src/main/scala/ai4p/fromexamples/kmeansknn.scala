package ai4p.fromexamples
import com.wbillingsley.veautiful.html.*
import com.wbillingsley.veautiful.doctacular.DeckBuilder

import <._
import ^._

import ai4p.{*, given}
import Common.*

import site.given
import scala.util.Random

import site.given
import scala.util.Random

import org.scalajs.dom

object KNNWidget {

  val w = 500
  val h = 500
  val pad = 40

  val classColors = Vector("#3b82f6", "#ef4444", "#22c55e")
  val classNames = Vector("Class A", "Class B", "Class C")

  val styling = Styling(
    """|display: inline-block;
       |font-family: 'Lato', sans-serif;
       |""".stripMargin
  ).modifiedBy(
    " svg"           -> "background: #f8f8f8; border: 1px solid #ddd; border-radius: 4px; cursor: crosshair;",
    " .knn-controls" -> "margin-top: 8px; display: flex; gap: 8px; align-items: center; flex-wrap: wrap;",
    " .knn-info"     -> "font-family: 'Playfair Design', serif; font-style: italic; font-size: 1.05rem; color: #5a074f; margin-top: 6px;",
    " .knn-legend"   -> "font-size: 0.8rem; color: #666; margin-top: 4px; display: flex; gap: 14px; flex-wrap: wrap;",
    " .knn-legend-item" -> "display: inline-flex; align-items: center;",
    " .knn-swatch"   -> "display: inline-block; width: 10px; height: 10px; border-radius: 50%; margin-right: 5px;",
    " .knn-label"    -> "font-size: 0.85rem; color: #666;",
    " input[type=range]" -> "width: 120px;"
  ).register()

  /** Map data x/y in [0,1] to SVG coordinates (square canvas, shared scale) */
  def toSvgX(x: Double): Double = pad + x * (w - 2 * pad)
  def toSvgY(y: Double): Double = (h - pad) - y * (h - 2 * pad)

  def dist(a: (Double, Double), b: (Double, Double)): Double =
    val dx = a._1 - b._1; val dy = a._2 - b._2
    math.sqrt(dx * dx + dy * dy)

  /** Finds the k nearest labelled points to `query`, and the majority-vote class among them */
  def classify(pts: Seq[(Double, Double, Int)], query: (Double, Double), k: Int)
      : (Int, Seq[(Double, Double, Int, Double)]) =
    val withDist = pts.map { case (x, y, c) => (x, y, c, dist((x, y), query)) }
    val nearest = withDist.sortBy(_._4).take(math.max(1, math.min(k, pts.size)))
    val vote = nearest.groupBy(_._3).view.mapValues(_.size).toSeq.sortBy(-_._2).headOption.map(_._1).getOrElse(0)
    (vote, nearest)

}

case class KNNWidget() extends DHtmlComponent {

  import KNNWidget._

  // Labelled training points: (x in [0,1], y in [0,1], class)
  var points: Seq[(Double, Double, Int)] = Seq(
    (0.20, 0.20, 0), (0.25, 0.35, 0), (0.15, 0.40, 0), (0.30, 0.15, 0),
    (0.75, 0.70, 1), (0.80, 0.55, 1), (0.65, 0.80, 1), (0.85, 0.75, 1),
    (0.70, 0.20, 2), (0.80, 0.15, 2), (0.60, 0.30, 2), (0.85, 0.30, 2)
  )

  var query: (Double, Double) = (0.5, 0.5)
  var activeClass: Int = 0
  var placingQuery: Boolean = false
  var k: Int = 3

  def toData(e: org.scalajs.dom.MouseEvent): Option[(Double, Double)] =
    val rect = e.currentTarget.asInstanceOf[org.scalajs.dom.svg.SVG].getBoundingClientRect()
    // rect is the on-screen (post CSS-transform) size, which may be scaled
    // relative to the SVG's own w x h coordinate space (e.g. inside a scaled slide deck)
    val svgX = (e.clientX - rect.left) * w / rect.width
    val svgY = (e.clientY - rect.top) * h / rect.height
    val px = (svgX - pad) / (w - 2 * pad)
    val py = 1.0 - (svgY - pad) / (h - 2 * pad)
    if px >= 0 && px <= 1 && py >= 0 && py <= 1 then Some((px, py)) else None

  def handleClick(e: org.scalajs.dom.MouseEvent): Unit =
    toData(e).foreach { p =>
      if placingQuery then query = p
      else points = points :+ (p._1, p._2, activeClass)
    }
    rerender()

  override protected def render =
    val (predicted, nearest) = classify(points, query, k)
    val maxDist = if nearest.nonEmpty then nearest.map(_._4).max else 0.0

    <.div(^.cls := styling.className,
      SVG.svg(
        ^.attr("width") := w, ^.attr("height") := h,
        ^.onClick ==> handleClick,

        SVG.rect(^.attr("x") := pad, ^.attr("y") := pad,
          ^.attr("width") := w - 2 * pad, ^.attr("height") := h - 2 * pad,
          ^.attr("fill") := "none", ^.attr("stroke") := "#ccc"),

        // Neighbourhood circle out to the k-th nearest neighbour
        SVG.circle(
          ^.attr("cx") := toSvgX(query._1), ^.attr("cy") := toSvgY(query._2),
          ^.attr("r") := maxDist * (w - 2 * pad),
          ^.attr("fill") := "#5a074f", ^.attr("fill-opacity") := "0.06",
          ^.attr("stroke") := "#5a074f", ^.attr("stroke-dasharray") := "4 3"
        ),

        // Lines from the query point to its k nearest neighbours
        for (nx, ny, _, _) <- nearest yield
          SVG.line(
            ^.attr("x1") := toSvgX(query._1), ^.attr("y1") := toSvgY(query._2),
            ^.attr("x2") := toSvgX(nx), ^.attr("y2") := toSvgY(ny),
            ^.attr("stroke") := "#5a074f", ^.attr("stroke-width") := "1", ^.attr("opacity") := "0.5"
          ),

        // Training points
        for (x, y, c) <- points yield
          SVG.circle(
            ^.attr("cx") := toSvgX(x), ^.attr("cy") := toSvgY(y),
            ^.attr("r") := "6",
            ^.attr("fill") := classColors(c),
            ^.attr("stroke") := "white", ^.attr("stroke-width") := "1.5"
          ),

        // Query point, coloured by its predicted class
        SVG.circle(
          ^.attr("cx") := toSvgX(query._1), ^.attr("cy") := toSvgY(query._2),
          ^.attr("r") := "9",
          ^.attr("fill") := classColors(predicted),
          ^.attr("stroke") := "black", ^.attr("stroke-width") := "2"
        )
      ),

      <.div(^.cls := "knn-info",
        s"k = $k nearest neighbours → predicted: ${classNames(predicted)}"
      ),

      <.div(^.cls := "knn-legend",
        for (name, i) <- classNames.zipWithIndex yield
          <.span(^.cls := "knn-legend-item",
            <.span(^.cls := "knn-swatch", ^.style := s"background: ${classColors(i)};"), name)
      ),

      <.div(^.cls := "knn-controls",
        <.span("Place:"),
        for (name, i) <- classNames.zipWithIndex yield
          <.button(
            ^.cls := (if !placingQuery && activeClass == i then "btn btn-primary btn-sm" else "btn btn-outline-primary btn-sm"),
            ^.onClick --> { placingQuery = false; activeClass = i; rerender() },
            name
          ),
        <.button(
          ^.cls := (if placingQuery then "btn btn-warning btn-sm" else "btn btn-outline-warning btn-sm"),
          ^.onClick --> { placingQuery = true; rerender() },
          "Place query point ✕"
        ),
        <.button(^.cls := "btn btn-outline-secondary btn-sm",
          ^.onClick --> { points = Seq.empty; rerender() },
          "Clear points"
        ),

        <.span(^.cls := "knn-label", "k ="),
        <("input")(
          ^.attr("type") := "range",
          ^.attr("min") := "1", ^.attr("max") := "11", ^.attr("step") := "1",
          ^.attr("value") := k.toString,
          ^.on("input") ==> { (e: org.scalajs.dom.Event) =>
            k = e.target.asInstanceOf[org.scalajs.dom.html.Input].value.toInt
            rerender()
          }
        ),
        <.span(^.cls := "knn-label", k.toString)
      )
    )
}

object KNNBoundaryWidget {

  val styling = Styling(
    """|display: inline-block;
       |font-family: 'Lato', sans-serif;
       |""".stripMargin
  ).modifiedBy(
    " svg"           -> "background: #f8f8f8; border: 1px solid #ddd; border-radius: 4px; cursor: crosshair;",
    " .knnb-controls" -> "margin-top: 8px; display: flex; gap: 8px; align-items: center; flex-wrap: wrap;",
    " .knnb-info"     -> "font-family: 'Playfair Design', serif; font-style: italic; font-size: 1.05rem; color: #5a074f; margin-top: 6px;",
    " .knnb-legend"   -> "font-size: 0.8rem; color: #666; margin-top: 4px; display: flex; gap: 14px; flex-wrap: wrap;",
    " .knnb-legend-item" -> "display: inline-flex; align-items: center;",
    " .knnb-swatch"   -> "display: inline-block; width: 10px; height: 10px; border-radius: 50%; margin-right: 5px;",
    " .knnb-label"    -> "font-size: 0.85rem; color: #666;",
    " input[type=range]" -> "width: 120px;"
  ).register()

}

case class KNNBoundaryWidget(initialResolution: Int = 40) extends DHtmlComponent {

  import KNNBoundaryWidget._
  import KNNWidget.{w, h, pad, classColors, classNames, toSvgX, toSvgY, dist, classify}

  // Labelled training points: (x in [0,1], y in [0,1], class)
  var points: Seq[(Double, Double, Int)] = Seq(
    (0.20, 0.20, 0), (0.25, 0.35, 0), (0.15, 0.40, 0), (0.30, 0.15, 0),
    (0.75, 0.70, 1), (0.80, 0.55, 1), (0.65, 0.80, 1), (0.85, 0.75, 1),
    (0.70, 0.20, 2), (0.80, 0.15, 2), (0.60, 0.30, 2), (0.85, 0.30, 2)
  )

  var activeClass: Int = 0
  var k: Int = 3
  var resolution: Int = initialResolution

  def toData(e: org.scalajs.dom.MouseEvent): Option[(Double, Double)] =
    val rect = e.currentTarget.asInstanceOf[org.scalajs.dom.svg.SVG].getBoundingClientRect()
    val svgX = (e.clientX - rect.left) * w / rect.width
    val svgY = (e.clientY - rect.top) * h / rect.height
    val px = (svgX - pad) / (w - 2 * pad)
    val py = 1.0 - (svgY - pad) / (h - 2 * pad)
    if px >= 0 && px <= 1 && py >= 0 && py <= 1 then Some((px, py)) else None

  def handleClick(e: org.scalajs.dom.MouseEvent): Unit =
    toData(e).foreach { p => points = points :+ (p._1, p._2, activeClass) }
    rerender()

  override protected def render =
    val cellSize = (w - 2 * pad).toDouble / resolution

    <.div(^.cls := styling.className,
      SVG.svg(
        ^.attr("width") := w, ^.attr("height") := h,
        ^.onClick ==> handleClick,

        // One pixel per grid cell, coloured by whatever class a query at its centre would be given
        if points.nonEmpty then
          for
            gx <- 0 until resolution
            gy <- 0 until resolution
          yield
            val cx = (gx + 0.5) / resolution
            val cy = (gy + 0.5) / resolution
            val predicted = classify(points, (cx, cy), k)._1
            SVG.rect(
              ^.attr("x") := toSvgX(cx) - cellSize / 2,
              ^.attr("y") := toSvgY(cy) - cellSize / 2,
              ^.attr("width") := cellSize + 0.5,
              ^.attr("height") := cellSize + 0.5,
              ^.attr("fill") := classColors(predicted),
              ^.attr("fill-opacity") := "0.35",
              ^.attr("stroke") := "none"
            )
        else SVG.g(),

        SVG.rect(^.attr("x") := pad, ^.attr("y") := pad,
          ^.attr("width") := w - 2 * pad, ^.attr("height") := h - 2 * pad,
          ^.attr("fill") := "none", ^.attr("stroke") := "#ccc"),

        // Training points
        for (x, y, c) <- points yield
          SVG.circle(
            ^.attr("cx") := toSvgX(x), ^.attr("cy") := toSvgY(y),
            ^.attr("r") := "6",
            ^.attr("fill") := classColors(c),
            ^.attr("stroke") := "white", ^.attr("stroke-width") := "1.5"
          )
      ),

      <.div(^.cls := "knnb-info",
        if points.isEmpty then "Click the chart to add labelled points"
        else s"k = $k nearest neighbours — every pixel shows what it would be classified as"
      ),

      <.div(^.cls := "knnb-legend",
        for (name, i) <- classNames.zipWithIndex yield
          <.span(^.cls := "knnb-legend-item",
            <.span(^.cls := "knnb-swatch", ^.style := s"background: ${classColors(i)};"), name)
      ),

      <.div(^.cls := "knnb-controls",
        <.span("Place:"),
        for (name, i) <- classNames.zipWithIndex yield
          <.button(
            ^.cls := (if activeClass == i then "btn btn-primary btn-sm" else "btn btn-outline-primary btn-sm"),
            ^.onClick --> { activeClass = i; rerender() },
            name
          ),
        <.button(^.cls := "btn btn-outline-secondary btn-sm",
          ^.onClick --> { points = Seq.empty; rerender() },
          "Clear points"
        ),

        <.span(^.cls := "knnb-label", "k ="),
        <("input")(
          ^.attr("type") := "range",
          ^.attr("min") := "1", ^.attr("max") := "11", ^.attr("step") := "1",
          ^.attr("value") := k.toString,
          ^.on("input") ==> { (e: org.scalajs.dom.Event) =>
            k = e.target.asInstanceOf[org.scalajs.dom.html.Input].value.toInt
            rerender()
          }
        ),
        <.span(^.cls := "knnb-label", k.toString),

        <.span(^.cls := "knnb-label", "resolution ="),
        <("input")(
          ^.attr("type") := "range",
          ^.attr("min") := "10", ^.attr("max") := "70", ^.attr("step") := "2",
          ^.attr("value") := resolution.toString,
          ^.on("input") ==> { (e: org.scalajs.dom.Event) =>
            resolution = e.target.asInstanceOf[org.scalajs.dom.html.Input].value.toInt
            rerender()
          }
        ),
        <.span(^.cls := "knnb-label", resolution.toString)
      )
    )
}

object VoronoiWidget {

  val styling = Styling(
    """|display: inline-block;
       |font-family: 'Lato', sans-serif;
       |""".stripMargin
  ).modifiedBy(
    " svg"            -> "background: #f8f8f8; border: 1px solid #ddd; border-radius: 4px; cursor: crosshair;",
    " .vor-controls"  -> "margin-top: 8px; display: flex; gap: 8px; align-items: center; flex-wrap: wrap;",
    " .vor-info"      -> "font-family: 'Playfair Design', serif; font-style: italic; font-size: 1.05rem; color: #5a074f; margin-top: 6px;",
    " .vor-legend"    -> "font-size: 0.8rem; color: #666; margin-top: 4px; display: flex; gap: 14px; flex-wrap: wrap;",
    " .vor-legend-item" -> "display: inline-flex; align-items: center;",
    " .vor-swatch"    -> "display: inline-block; width: 10px; height: 10px; border-radius: 50%; margin-right: 5px;",
    " .vor-label"     -> "font-size: 0.85rem; color: #666;"
  ).register()

  /** A distinct, stable pastel colour per site index, spaced round the hue wheel by the golden angle */
  def siteColor(i: Int): String =
    val hue = (i * 137.508) % 360.0
    f"hsl($hue%.0f, 65%%, 82%%)"

  /** The half-plane of points at least as close to `p` as to `q`, expressed as n·x <= c */
  def bisectorHalfPlane(p: (Double, Double), q: (Double, Double)): ((Double, Double), Double) =
    val n = (q._1 - p._1, q._2 - p._2)
    val c = (q._1 * q._1 + q._2 * q._2 - p._1 * p._1 - p._2 * p._2) / 2.0
    (n, c)

  /** Clips a convex polygon to one half-plane n·x <= c, via Sutherland-Hodgman */
  def clipHalfPlane(poly: Vector[(Double, Double)], n: (Double, Double), c: Double): Vector[(Double, Double)] =
    def side(pt: (Double, Double)): Double = n._1 * pt._1 + n._2 * pt._2 - c
    def intersect(a: (Double, Double), b: (Double, Double)): (Double, Double) =
      val t = side(a) / (side(a) - side(b))
      (a._1 + t * (b._1 - a._1), a._2 + t * (b._2 - a._2))

    if poly.isEmpty then poly
    else
      val out = scala.collection.mutable.ArrayBuffer[(Double, Double)]()
      for i <- poly.indices do
        val curr = poly(i)
        val prev = poly((i - 1 + poly.size) % poly.size)
        val currIn = side(curr) <= 1e-9
        val prevIn = side(prev) <= 1e-9
        if currIn then
          if !prevIn then out += intersect(prev, curr)
          out += curr
        else if prevIn then
          out += intersect(prev, curr)
      out.toVector

  /** The Voronoi cell for site `i`, as a convex polygon clipped down from `bounds` */
  def voronoiCell(sites: Seq[(Double, Double)], i: Int, bounds: Vector[(Double, Double)]): Vector[(Double, Double)] =
    sites.indices.foldLeft(bounds) { (poly, j) =>
      if j == i || poly.isEmpty then poly
      else
        val (n, c) = bisectorHalfPlane(sites(i), sites(j))
        clipHalfPlane(poly, n, c)
    }

}

case class VoronoiWidget() extends DHtmlComponent {

  import VoronoiWidget._
  import KNNWidget.{w, h, pad, classColors, classNames, toSvgX, toSvgY}

  // Labelled training points: (x in [0,1], y in [0,1], class)
  var points: Seq[(Double, Double, Int)] = Seq(
    (0.20, 0.20, 0), (0.25, 0.35, 0), (0.15, 0.40, 0), (0.30, 0.15, 0),
    (0.75, 0.70, 1), (0.80, 0.55, 1), (0.65, 0.80, 1), (0.85, 0.75, 1),
    (0.70, 0.20, 2), (0.80, 0.15, 2), (0.60, 0.30, 2), (0.85, 0.30, 2)
  )

  var activeClass: Int = 0
  var colourByClass: Boolean = false

  val bounds: Vector[(Double, Double)] = Vector((0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 1.0))

  def toData(e: org.scalajs.dom.MouseEvent): Option[(Double, Double)] =
    val rect = e.currentTarget.asInstanceOf[org.scalajs.dom.svg.SVG].getBoundingClientRect()
    val svgX = (e.clientX - rect.left) * w / rect.width
    val svgY = (e.clientY - rect.top) * h / rect.height
    val px = (svgX - pad) / (w - 2 * pad)
    val py = 1.0 - (svgY - pad) / (h - 2 * pad)
    if px >= 0 && px <= 1 && py >= 0 && py <= 1 then Some((px, py)) else None

  def handleClick(e: org.scalajs.dom.MouseEvent): Unit =
    toData(e).foreach { p => points = points :+ (p._1, p._2, activeClass) }
    rerender()

  def toPointsAttr(poly: Vector[(Double, Double)]): String =
    poly.map { case (x, y) => s"${toSvgX(x)},${toSvgY(y)}" }.mkString(" ")

  override protected def render =
    val sites = points.map { case (x, y, _) => (x, y) }

    <.div(^.cls := styling.className,
      SVG.svg(
        ^.attr("width") := w, ^.attr("height") := h,
        ^.onClick ==> handleClick,

        // One convex polygon per site — the region of the plane closer to it than to any other site
        for i <- points.indices yield
          val cell = voronoiCell(sites, i, bounds)
          val (_, _, cls) = points(i)
          SVG.polygon(
            ^.attr("points") := toPointsAttr(cell),
            ^.attr("fill") := (if colourByClass then classColors(cls) else siteColor(i)),
            ^.attr("fill-opacity") := (if colourByClass then "0.45" else "1"),
            ^.attr("stroke") := (if colourByClass then "none" else "#999"),
            ^.attr("stroke-width") := "1"
          ),

        SVG.rect(^.attr("x") := pad, ^.attr("y") := pad,
          ^.attr("width") := w - 2 * pad, ^.attr("height") := h - 2 * pad,
          ^.attr("fill") := "none", ^.attr("stroke") := "#ccc"),

        // Training points (the Voronoi sites)
        for (x, y, c) <- points yield
          SVG.circle(
            ^.attr("cx") := toSvgX(x), ^.attr("cy") := toSvgY(y),
            ^.attr("r") := "5",
            ^.attr("fill") := classColors(c),
            ^.attr("stroke") := "black", ^.attr("stroke-width") := "1.5"
          )
      ),

      <.div(^.cls := "vor-info",
        if points.isEmpty then "Click the chart to add labelled points"
        else if colourByClass then "Cells grouped by class — this is exactly the 1-NN decision boundary"
        else "One cell per point — the region closer to it than to any other point"
      ),

      <.div(^.cls := "vor-legend",
        for (name, i) <- classNames.zipWithIndex yield
          <.span(^.cls := "vor-legend-item",
            <.span(^.cls := "vor-swatch", ^.style := s"background: ${classColors(i)};"), name)
      ),

      <.div(^.cls := "vor-controls",
        <.span("Place:"),
        for (name, i) <- classNames.zipWithIndex yield
          <.button(
            ^.cls := (if activeClass == i then "btn btn-primary btn-sm" else "btn btn-outline-primary btn-sm"),
            ^.onClick --> { activeClass = i; rerender() },
            name
          ),
        <.button(^.cls := "btn btn-outline-secondary btn-sm",
          ^.onClick --> { points = Seq.empty; rerender() },
          "Clear points"
        ),
        <.button(
          ^.cls := (if colourByClass then "btn btn-warning btn-sm" else "btn btn-outline-warning btn-sm"),
          ^.onClick --> { colourByClass = !colourByClass; rerender() },
          if colourByClass then "Colour: by class" else "Colour: by site"
        )
      )
    )
}

object KMeansWidget {

  val w = 500
  val h = 500
  val pad = 40

  val clusterColors = Vector("#3b82f6", "#ef4444", "#22c55e", "#f59e0b", "#a855f7")

  val styling = Styling(
    """|display: inline-block;
       |font-family: 'Lato', sans-serif;
       |""".stripMargin
  ).modifiedBy(
    " svg"          -> "background: #f8f8f8; border: 1px solid #ddd; border-radius: 4px;",
    " .km-controls" -> "margin-top: 8px; display: flex; gap: 8px; align-items: center; flex-wrap: wrap;",
    " .km-info"     -> "font-family: 'Playfair Design', serif; font-style: italic; font-size: 1.05rem; color: #5a074f; margin-top: 6px;",
    " .km-status"   -> "font-size: 0.85rem; padding: 2px 8px; border-radius: 4px; font-weight: bold;",
    " .km-status.converged" -> "background: #dcfce7; color: #166534;",
    " .km-status.moving"    -> "background: #fef9c3; color: #854d0e;"
  ).register()

  def toSvgX(x: Double): Double = pad + x * (w - 2 * pad)
  def toSvgY(y: Double): Double = (h - pad) - y * (h - 2 * pad)

  def dist(a: (Double, Double), b: (Double, Double)): Double =
    val dx = a._1 - b._1; val dy = a._2 - b._2
    math.sqrt(dx * dx + dy * dy)

  /** Roughly gaussian jitter in about [-spread, spread], via the sum of a few uniforms */
  def jitter(spread: Double): Double =
    (Random.nextDouble() + Random.nextDouble() + Random.nextDouble() - 1.5) / 1.5 * spread

  /** A fresh dataset of k blobs scattered around random centres in [0,1] x [0,1] */
  def randomBlobs(k: Int, perCluster: Int = 12): Seq[(Double, Double)] =
    val centres = (0 until k).map(_ => (0.15 + Random.nextDouble() * 0.7, 0.15 + Random.nextDouble() * 0.7))
    centres.flatMap { case (cx, cy) =>
      (0 until perCluster).map { _ =>
        val x = math.max(0.02, math.min(0.98, cx + jitter(0.12)))
        val y = math.max(0.02, math.min(0.98, cy + jitter(0.12)))
        (x, y)
      }
    }

}

case class KMeansWidget(initialK: Int = 3) extends DHtmlComponent {

  import KMeansWidget._

  var k: Int = initialK
  var points: Seq[(Double, Double)] = randomBlobs(k)
  var centroids: Seq[(Double, Double)] = Random.shuffle(points).take(k)
  var autoRunning = false
  var timerId: Option[Int] = None
  var lastMovement: Double = Double.MaxValue

  def assignments: Seq[Int] =
    points.map { p => centroids.indices.minBy(i => dist(p, centroids(i))) }

  def reinitCentroids(): Unit =
    centroids = Random.shuffle(points).take(k)
    lastMovement = Double.MaxValue

  def newPoints(): Unit =
    stopAuto()
    points = randomBlobs(k)
    reinitCentroids()

  def setK(newK: Int): Unit =
    stopAuto()
    k = newK
    points = randomBlobs(k)
    reinitCentroids()

  /** One assign+update cycle of Lloyd's algorithm */
  def step(): Unit =
    val assign = assignments
    val newCentroids = centroids.indices.map { i =>
      val members = points.zip(assign).collect { case (p, ci) if ci == i => p }
      if members.isEmpty then centroids(i)
      else (members.map(_._1).sum / members.size, members.map(_._2).sum / members.size)
    }
    lastMovement = centroids.zip(newCentroids).map { case (a, b) => dist(a, b) }.max
    centroids = newCentroids
    if lastMovement < 0.001 then stopAuto()

  def stopAuto(): Unit =
    timerId.foreach(dom.window.clearInterval(_))
    timerId = None
    autoRunning = false

  def toggleAuto(): Unit =
    if autoRunning then
      stopAuto()
      rerender()
    else
      autoRunning = true
      timerId = Some(dom.window.setInterval(() => {
        step()
        rerender()
      }, 600))
      rerender()

  override def afterDetach(): Unit = stopAuto()

  override protected def render =
    val assign = assignments
    val status = if lastMovement < 0.001 then "converged" else "moving"
    val statusLabel = if status == "converged" then "Converged ✓" else "Moving…"

    <.div(^.cls := styling.className,
      SVG.svg(^.attr("width") := w, ^.attr("height") := h,

        SVG.rect(^.attr("x") := pad, ^.attr("y") := pad,
          ^.attr("width") := w - 2 * pad, ^.attr("height") := h - 2 * pad,
          ^.attr("fill") := "none", ^.attr("stroke") := "#ccc"),

        // Lines from each point to the centroid it's currently assigned to
        for (p, ci) <- points.zip(assign) yield
          SVG.line(
            ^.attr("x1") := toSvgX(p._1), ^.attr("y1") := toSvgY(p._2),
            ^.attr("x2") := toSvgX(centroids(ci)._1), ^.attr("y2") := toSvgY(centroids(ci)._2),
            ^.attr("stroke") := clusterColors(ci % clusterColors.size),
            ^.attr("stroke-width") := "1", ^.attr("opacity") := "0.25"
          ),

        // Data points, coloured by current cluster assignment
        for (p, ci) <- points.zip(assign) yield
          SVG.circle(
            ^.attr("cx") := toSvgX(p._1), ^.attr("cy") := toSvgY(p._2),
            ^.attr("r") := "5",
            ^.attr("fill") := clusterColors(ci % clusterColors.size),
            ^.attr("stroke") := "white", ^.attr("stroke-width") := "1"
          ),

        // Centroids, drawn as a black-outlined cross in the cluster's colour
        for (c, i) <- centroids.zipWithIndex yield
          SVG.g(
            SVG.line(^.attr("x1") := toSvgX(c._1) - 8, ^.attr("y1") := toSvgY(c._2) - 8,
                     ^.attr("x2") := toSvgX(c._1) + 8, ^.attr("y2") := toSvgY(c._2) + 8,
                     ^.attr("stroke") := "black", ^.attr("stroke-width") := "3"),
            SVG.line(^.attr("x1") := toSvgX(c._1) - 8, ^.attr("y1") := toSvgY(c._2) + 8,
                     ^.attr("x2") := toSvgX(c._1) + 8, ^.attr("y2") := toSvgY(c._2) - 8,
                     ^.attr("stroke") := "black", ^.attr("stroke-width") := "3"),
            SVG.line(^.attr("x1") := toSvgX(c._1) - 7, ^.attr("y1") := toSvgY(c._2) - 7,
                     ^.attr("x2") := toSvgX(c._1) + 7, ^.attr("y2") := toSvgY(c._2) + 7,
                     ^.attr("stroke") := clusterColors(i % clusterColors.size), ^.attr("stroke-width") := "2"),
            SVG.line(^.attr("x1") := toSvgX(c._1) - 7, ^.attr("y1") := toSvgY(c._2) + 7,
                     ^.attr("x2") := toSvgX(c._1) + 7, ^.attr("y2") := toSvgY(c._2) - 7,
                     ^.attr("stroke") := clusterColors(i % clusterColors.size), ^.attr("stroke-width") := "2")
          )
      ),

      <.div(^.cls := "km-info",
        f"k = $k clusters   —   max centroid movement last step: $lastMovement%.4f"
      ),

      <.div(^.cls := "km-controls",
        <.span("k ="),
        for kk <- 2 to 5 yield
          <.button(
            ^.cls := (if k == kk then "btn btn-primary btn-sm" else "btn btn-outline-primary btn-sm"),
            ^.onClick --> { setK(kk); rerender() },
            kk.toString
          ),
        <.button(^.cls := "btn btn-outline-primary btn-sm",
          ^.onClick --> { step(); rerender() }, "Step"),
        <.button(^.cls := (if autoRunning then "btn btn-warning btn-sm" else "btn btn-outline-success btn-sm"),
          ^.onClick --> toggleAuto(),
          if autoRunning then "Stop" else "Auto"),
        <.button(^.cls := "btn btn-outline-secondary btn-sm",
          ^.onClick --> { reinitCentroids(); rerender() }, "Reset centroids"),
        <.button(^.cls := "btn btn-outline-secondary btn-sm",
          ^.onClick --> { newPoints(); rerender() }, "New points"),

        <.span(^.cls := s"km-status $status", statusLabel)
      )
    )
}

object ElbowWidget {

  val chartW = 420
  val chartH = 340
  val chartPad = 40

  val panelW = 340
  val panelH = 340
  val panelPad = 30

  val maxK = 8
  val restarts = 8

  val clusterColors = KMeansWidget.clusterColors
  val dist: ((Double, Double), (Double, Double)) => Double = KMeansWidget.dist
  val jitter: Double => Double = KMeansWidget.jitter

  val styling = Styling(
    """|display: inline-block;
       |font-family: 'Lato', sans-serif;
       |""".stripMargin
  ).modifiedBy(
    " svg"             -> "background: #f8f8f8; border: 1px solid #ddd; border-radius: 4px;",
    " .elbow-row"       -> "display: flex; gap: 16px; align-items: flex-start;",
    " .elbow-controls"  -> "margin-top: 8px; display: flex; gap: 8px; align-items: center; flex-wrap: wrap;",
    " .elbow-info"      -> "font-family: 'Playfair Design', serif; font-style: italic; font-size: 1.05rem; color: #5a074f; margin-top: 6px;",
    " .elbow-axis"      -> "font-size: 0.75rem; fill: #666;"
  ).register()

  /** A fresh dataset of `trueK` blobs scattered around random centres in [0,1] x [0,1] */
  def randomBlobs(trueK: Int, perCluster: Int = 12): Seq[(Double, Double)] =
    val centres = (0 until trueK).map(_ => (0.15 + Random.nextDouble() * 0.7, 0.15 + Random.nextDouble() * 0.7))
    centres.flatMap { case (cx, cy) =>
      (0 until perCluster).map { _ =>
        val x = math.max(0.02, math.min(0.98, cx + jitter(0.12)))
        val y = math.max(0.02, math.min(0.98, cy + jitter(0.12)))
        (x, y)
      }
    }

  /** Runs Lloyd's algorithm to convergence (or maxIter) from the given starting centroids */
  def lloyd(points: Seq[(Double, Double)], initCentroids: Seq[(Double, Double)], maxIter: Int = 50)
      : (Seq[(Double, Double)], Seq[Int]) =
    var centroids = initCentroids
    var assign = Seq.empty[Int]
    var iter = 0
    var moving = true
    while iter < maxIter && moving do
      assign = points.map(p => centroids.indices.minBy(i => dist(p, centroids(i))))
      val newCentroids = centroids.indices.map { i =>
        val members = points.zip(assign).collect { case (p, ci) if ci == i => p }
        if members.isEmpty then centroids(i)
        else (members.map(_._1).sum / members.size, members.map(_._2).sum / members.size)
      }
      moving = centroids.zip(newCentroids).exists { case (a, b) => dist(a, b) > 1e-6 }
      centroids = newCentroids
      iter += 1
    (centroids, assign)

  /** Total within-cluster sum of squared distances — what the elbow method plots against k */
  def wcss(points: Seq[(Double, Double)], centroids: Seq[(Double, Double)], assign: Seq[Int]): Double =
    points.zip(assign).map { case (p, ci) => val d = dist(p, centroids(ci)); d * d }.sum

  /** Best of several random-restart runs of k-means for a given k, by lowest WCSS */
  def bestKMeans(points: Seq[(Double, Double)], k: Int): (Seq[(Double, Double)], Seq[Int], Double) =
    val kk = math.max(1, math.min(k, points.size))
    val attempts = (0 until restarts).map { _ =>
      val init = Random.shuffle(points).take(kk)
      val (c, a) = lloyd(points, init)
      (c, a, wcss(points, c, a))
    }
    attempts.minBy(_._3)

  /** The elbow: the k whose (k, wcss) point sits furthest from the chord joining k=1 and k=maxK */
  def findElbow(wcssByK: Seq[(Int, Double)]): Int =
    if wcssByK.size < 3 then wcssByK.headOption.map(_._1).getOrElse(1)
    else
      val ks = wcssByK.map(_._1.toDouble)
      val ws = wcssByK.map(_._2)
      val (kMin, kMax) = (ks.min, ks.max)
      val (wMin, wMax) = (ws.min, ws.max)
      def norm(k: Int, w: Double): (Double, Double) =
        (if kMax > kMin then (k - kMin) / (kMax - kMin) else 0.0,
         if wMax > wMin then (w - wMin) / (wMax - wMin) else 0.0)
      val (x1, y1) = norm(wcssByK.head._1, wcssByK.head._2)
      val (x2, y2) = norm(wcssByK.last._1, wcssByK.last._2)
      val den = math.sqrt((y2 - y1) * (y2 - y1) + (x2 - x1) * (x2 - x1))
      wcssByK.maxBy { case (k, w) =>
        val (px, py) = norm(k, w)
        if den == 0 then 0.0 else math.abs((y2 - y1) * px - (x2 - x1) * py + x2 * y1 - y2 * x1) / den
      }._1

}

case class ElbowWidget(trueK: Int = 4) extends DHtmlComponent {

  import ElbowWidget._

  var points: Seq[(Double, Double)] = randomBlobs(trueK)

  // (k, centroids, assignments, wcss) for k = 1 .. maxK, best of several restarts each
  var results: Vector[(Int, Seq[(Double, Double)], Seq[Int], Double)] = Vector.empty
  var elbowK: Int = 1
  var selectedK: Int = 1

  def recompute(): Unit =
    results = (1 to maxK).map { k =>
      val (c, a, w) = bestKMeans(points, k)
      (k, c, a, w)
    }.toVector
    elbowK = findElbow(results.map { case (k, _, _, w) => (k, w) })
    selectedK = elbowK

  def newPoints(): Unit =
    points = randomBlobs(trueK)
    recompute()

  recompute()

  def chartX(k: Int): Double = chartPad + (k - 1).toDouble / (maxK - 1) * (chartW - 2 * chartPad)
  def chartY(w: Double, wMax: Double): Double =
    if wMax <= 0 then chartH - chartPad
    else (chartH - chartPad) - (w / wMax) * (chartH - 2 * chartPad)

  def panelX(x: Double): Double = panelPad + x * (panelW - 2 * panelPad)
  def panelY(y: Double): Double = (panelH - panelPad) - y * (panelH - 2 * panelPad)

  override protected def render =
    val wMax = results.map(_._4).max
    val selected = results(selectedK - 1)
    val (_, centroids, assign, _) = selected

    <.div(^.cls := styling.className,
      <.div(^.cls := "elbow-row",

        // Left: WCSS vs k, with the elbow highlighted
        SVG.svg(^.attr("width") := chartW, ^.attr("height") := chartH,
          SVG.rect(^.attr("x") := chartPad, ^.attr("y") := chartPad,
            ^.attr("width") := chartW - 2 * chartPad, ^.attr("height") := chartH - 2 * chartPad,
            ^.attr("fill") := "none", ^.attr("stroke") := "#ccc"),

          // The chord from k=1 to k=maxK that the elbow is measured against
          SVG.line(
            ^.attr("x1") := chartX(results.head._1), ^.attr("y1") := chartY(results.head._4, wMax),
            ^.attr("x2") := chartX(results.last._1), ^.attr("y2") := chartY(results.last._4, wMax),
            ^.attr("stroke") := "#bbb", ^.attr("stroke-dasharray") := "4 3"
          ),

          // The WCSS curve
          SVG.polyline(
            ^.attr("points") := results.map { case (k, _, _, w) => s"${chartX(k)},${chartY(w, wMax)}" }.mkString(" "),
            ^.attr("fill") := "none", ^.attr("stroke") := "#5a074f", ^.attr("stroke-width") := "2"
          ),

          // One clickable point per k, so you can compare the elbow's choice against neighbouring k
          for (k, _, _, w) <- results yield
            SVG.circle(
              ^.attr("cx") := chartX(k), ^.attr("cy") := chartY(w, wMax),
              ^.attr("r") := (if k == elbowK then "8" else "5"),
              ^.attr("fill") := (if k == selectedK then "#f59e0b" else if k == elbowK then "#5a074f" else "white"),
              ^.attr("stroke") := "#5a074f", ^.attr("stroke-width") := "2",
              ^.style := "cursor: pointer;",
              ^.onClick --> { selectedK = k; rerender() }
            ),

          // x-axis labels
          for (k, _, _, _) <- results yield
            SVG.text(^.cls := "elbow-axis",
              ^.attr("x") := chartX(k), ^.attr("y") := chartH - chartPad + 16,
              ^.attr("text-anchor") := "middle",
              k.toString
            ),
          SVG.text(^.cls := "elbow-axis", ^.attr("x") := chartW / 2, ^.attr("y") := chartH - 6,
            ^.attr("text-anchor") := "middle", "k"),
          SVG.text(^.cls := "elbow-axis",
            ^.attr("x") := 14, ^.attr("y") := chartH / 2,
            ^.attr("text-anchor") := "middle",
            ^.attr("transform") := s"rotate(-90, 14, ${chartH / 2})",
            "WCSS (inertia)"
          )
        ),

        // Right: what the clustering actually looks like at the selected k
        SVG.svg(^.attr("width") := panelW, ^.attr("height") := panelH,
          SVG.rect(^.attr("x") := panelPad, ^.attr("y") := panelPad,
            ^.attr("width") := panelW - 2 * panelPad, ^.attr("height") := panelH - 2 * panelPad,
            ^.attr("fill") := "none", ^.attr("stroke") := "#ccc"),

          for (p, ci) <- points.zip(assign) yield
            SVG.circle(
              ^.attr("cx") := panelX(p._1), ^.attr("cy") := panelY(p._2),
              ^.attr("r") := "5",
              ^.attr("fill") := clusterColors(ci % clusterColors.size),
              ^.attr("stroke") := "white", ^.attr("stroke-width") := "1"
            ),

          for (c, i) <- centroids.zipWithIndex yield
            SVG.g(
              SVG.line(^.attr("x1") := panelX(c._1) - 8, ^.attr("y1") := panelY(c._2) - 8,
                       ^.attr("x2") := panelX(c._1) + 8, ^.attr("y2") := panelY(c._2) + 8,
                       ^.attr("stroke") := "black", ^.attr("stroke-width") := "3"),
              SVG.line(^.attr("x1") := panelX(c._1) - 8, ^.attr("y1") := panelY(c._2) + 8,
                       ^.attr("x2") := panelX(c._1) + 8, ^.attr("y2") := panelY(c._2) - 8,
                       ^.attr("stroke") := "black", ^.attr("stroke-width") := "3"),
              SVG.line(^.attr("x1") := panelX(c._1) - 7, ^.attr("y1") := panelY(c._2) - 7,
                       ^.attr("x2") := panelX(c._1) + 7, ^.attr("y2") := panelY(c._2) + 7,
                       ^.attr("stroke") := clusterColors(i % clusterColors.size), ^.attr("stroke-width") := "2"),
              SVG.line(^.attr("x1") := panelX(c._1) - 7, ^.attr("y1") := panelY(c._2) + 7,
                       ^.attr("x2") := panelX(c._1) + 7, ^.attr("y2") := panelY(c._2) - 7,
                       ^.attr("stroke") := clusterColors(i % clusterColors.size), ^.attr("stroke-width") := "2")
            )
        )
      ),

      <.div(^.cls := "elbow-info",
        if selectedK == elbowK then s"Elbow method picks k = $elbowK"
        else s"Showing k = $selectedK — the elbow method would pick k = $elbowK (click its point to go back)"
      ),

      <.div(^.cls := "elbow-controls",
        <.button(^.cls := "btn btn-outline-secondary btn-sm",
          ^.onClick --> { newPoints(); rerender() },
          "New points"
        )
      )
    )
}

object DBSCANWidget {

  val w = 500
  val h = 500
  val pad = 40

  val noiseColor = "#9ca3af"
  val clusterColors = KMeansWidget.clusterColors
  val dist: ((Double, Double), (Double, Double)) => Double = KMeansWidget.dist
  val jitter: Double => Double = KMeansWidget.jitter

  val styling = Styling(
    """|display: inline-block;
       |font-family: 'Lato', sans-serif;
       |""".stripMargin
  ).modifiedBy(
    " svg"              -> "background: #f8f8f8; border: 1px solid #ddd; border-radius: 4px; cursor: crosshair;",
    " .dbscan-controls" -> "margin-top: 8px; display: flex; gap: 8px; align-items: center; flex-wrap: wrap;",
    " .dbscan-info"     -> "font-family: 'Playfair Design', serif; font-style: italic; font-size: 1.05rem; color: #5a074f; margin-top: 6px;",
    " .dbscan-legend"   -> "font-size: 0.8rem; color: #666; margin-top: 4px; display: flex; gap: 14px; flex-wrap: wrap; align-items: center;",
    " .dbscan-legend-item" -> "display: inline-flex; align-items: center;",
    " .dbscan-swatch"   -> "display: inline-block; width: 10px; height: 10px; border-radius: 50%; margin-right: 5px;",
    " .dbscan-label"    -> "font-size: 0.85rem; color: #666;",
    " input[type=range]" -> "width: 120px;"
  ).register()

  def toSvgX(x: Double): Double = pad + x * (w - 2 * pad)
  def toSvgY(y: Double): Double = (h - pad) - y * (h - 2 * pad)

  /** Two dense blobs plus a sprinkling of uniformly-scattered noise points */
  def randomBlobsWithNoise(blobs: Int = 2, perBlob: Int = 16, noisePoints: Int = 10): Seq[(Double, Double)] =
    val centres = (0 until blobs).map(_ => (0.2 + Random.nextDouble() * 0.6, 0.2 + Random.nextDouble() * 0.6))
    val clustered = centres.flatMap { case (cx, cy) =>
      (0 until perBlob).map { _ =>
        val x = math.max(0.02, math.min(0.98, cx + jitter(0.09)))
        val y = math.max(0.02, math.min(0.98, cy + jitter(0.09)))
        (x, y)
      }
    }
    val noise = (0 until noisePoints).map(_ => (Random.nextDouble(), Random.nextDouble()))
    clustered ++ noise

  /** Indices of all points within `eps` of `points(i)`, including `i` itself */
  def regionQuery(points: Seq[(Double, Double)], i: Int, eps: Double): Seq[Int] =
    points.indices.filter(j => dist(points(i), points(j)) <= eps)

  /**
   * Classic DBSCAN. Returns a label per point: -1 for noise, otherwise a cluster id from 0.
   * Points reachable from more than one cluster's core points keep the id of whichever visited them first.
   */
  def dbscan(points: Seq[(Double, Double)], eps: Double, minPts: Int): Seq[Int] =
    val UNVISITED = -2
    val NOISE = -1
    val labels = Array.fill(points.size)(UNVISITED)
    var nextCluster = 0

    for i <- points.indices do
      if labels(i) == UNVISITED then
        val neighbors = regionQuery(points, i, eps)
        if neighbors.size < minPts then
          labels(i) = NOISE
        else
          val clusterId = nextCluster
          nextCluster += 1
          labels(i) = clusterId
          val seeds = scala.collection.mutable.Queue.empty[Int] ++= neighbors.filterNot(_ == i)
          while seeds.nonEmpty do
            val j = seeds.dequeue()
            if labels(j) == NOISE then labels(j) = clusterId
            if labels(j) == UNVISITED then
              labels(j) = clusterId
              val jNeighbors = regionQuery(points, j, eps)
              if jNeighbors.size >= minPts then seeds ++= jNeighbors

    labels.toSeq

}

case class DBSCANWidget() extends DHtmlComponent {

  import DBSCANWidget._

  var points: Seq[(Double, Double)] = randomBlobsWithNoise()
  var eps: Double = 0.09
  var minPts: Int = 4

  def toData(e: org.scalajs.dom.MouseEvent): Option[(Double, Double)] =
    val rect = e.currentTarget.asInstanceOf[org.scalajs.dom.svg.SVG].getBoundingClientRect()
    val svgX = (e.clientX - rect.left) * w / rect.width
    val svgY = (e.clientY - rect.top) * h / rect.height
    val px = (svgX - pad) / (w - 2 * pad)
    val py = 1.0 - (svgY - pad) / (h - 2 * pad)
    if px >= 0 && px <= 1 && py >= 0 && py <= 1 then Some((px, py)) else None

  def handleClick(e: org.scalajs.dom.MouseEvent): Unit =
    toData(e).foreach { p => points = points :+ p }
    rerender()

  def newPoints(): Unit =
    points = randomBlobsWithNoise()

  override protected def render =
    val labels = if points.nonEmpty then dbscan(points, eps, minPts) else Seq.empty
    val isCore = points.indices.map(i => regionQuery(points, i, eps).size >= minPts)
    val numClusters = if labels.isEmpty then 0 else labels.filter(_ >= 0).distinct.size
    val numNoise = labels.count(_ == -1)

    def colorOf(label: Int): String = if label < 0 then noiseColor else clusterColors(label % clusterColors.size)

    <.div(^.cls := styling.className,
      SVG.svg(
        ^.attr("width") := w, ^.attr("height") := h,
        ^.onClick ==> handleClick,

        // eps-radius neighbourhoods, drawn faintly around every core point
        for i <- points.indices if isCore(i) yield
          SVG.circle(
            ^.attr("cx") := toSvgX(points(i)._1), ^.attr("cy") := toSvgY(points(i)._2),
            ^.attr("r") := eps * (w - 2 * pad),
            ^.attr("fill") := colorOf(labels(i)), ^.attr("fill-opacity") := "0.06",
            ^.attr("stroke") := "none"
          ),

        SVG.rect(^.attr("x") := pad, ^.attr("y") := pad,
          ^.attr("width") := w - 2 * pad, ^.attr("height") := h - 2 * pad,
          ^.attr("fill") := "none", ^.attr("stroke") := "#ccc"),

        // Points: core points as solid filled dots, border points outlined, noise as small grey dots
        for i <- points.indices yield
          val (x, y) = points(i)
          val label = labels(i)
          SVG.circle(
            ^.attr("cx") := toSvgX(x), ^.attr("cy") := toSvgY(y),
            ^.attr("r") := (if label < 0 then "4" else if isCore(i) then "7" else "6"),
            ^.attr("fill") := (if label < 0 || isCore(i) then colorOf(label) else "white"),
            ^.attr("stroke") := (if label < 0 then colorOf(label) else "black"),
            ^.attr("stroke-width") := (if isCore(i) && label >= 0 then "1.5" else "2")
          )
      ),

      <.div(^.cls := "dbscan-info",
        if points.isEmpty then "Click the chart to add points"
        else f"eps = $eps%.3f, minPts = $minPts → $numClusters clusters, $numNoise noise points"
      ),

      <.div(^.cls := "dbscan-legend",
        <.span(^.cls := "dbscan-legend-item",
          <.span(^.cls := "dbscan-swatch", ^.style := "background: black; border-radius: 50%;"), "core point"),
        <.span(^.cls := "dbscan-legend-item",
          <.span(^.cls := "dbscan-swatch", ^.style := "background: white; border: 1.5px solid black;"), "border point"),
        <.span(^.cls := "dbscan-legend-item",
          <.span(^.cls := "dbscan-swatch", ^.style := s"background: $noiseColor;"), "noise")
      ),

      <.div(^.cls := "dbscan-controls",
        <.button(^.cls := "btn btn-outline-secondary btn-sm",
          ^.onClick --> { newPoints(); rerender() },
          "New points"
        ),
        <.button(^.cls := "btn btn-outline-secondary btn-sm",
          ^.onClick --> { points = Seq.empty; rerender() },
          "Clear points"
        ),

        <.span(^.cls := "dbscan-label", "eps ="),
        <("input")(
          ^.attr("type") := "range",
          ^.attr("min") := "0.02", ^.attr("max") := "0.25", ^.attr("step") := "0.005",
          ^.attr("value") := eps.toString,
          ^.on("input") ==> { (e: org.scalajs.dom.Event) =>
            eps = e.target.asInstanceOf[org.scalajs.dom.html.Input].value.toDouble
            rerender()
          }
        ),
        <.span(^.cls := "dbscan-label", f"$eps%.3f"),

        <.span(^.cls := "dbscan-label", "minPts ="),
        <("input")(
          ^.attr("type") := "range",
          ^.attr("min") := "2", ^.attr("max") := "10", ^.attr("step") := "1",
          ^.attr("value") := minPts.toString,
          ^.on("input") ==> { (e: org.scalajs.dom.Event) =>
            minPts = e.target.asInstanceOf[org.scalajs.dom.html.Input].value.toInt
            rerender()
          }
        ),
        <.span(^.cls := "dbscan-label", minPts.toString)
      )
    )
}

val kmeansknn = DeckBuilder(1920, 1080)
  .markdownSlide(
    """
      |# K-Means and KNN
      |
      |""".stripMargin
  ).withClass("center middle")
  .markdownSlides(
    """
    |## K Nearest Neighbours (KNN)
    |
    |If we're learning from examples, then one of the simplest forms of classifications is "what's this most similar to"
    |
    |"K nearest neighbours" classifies a new data point by looking at the *k* closest examples it's already seen, and assuming
    |it should be classified however the majority of those were.
    |
    |
    |""".stripMargin)
  .veautifulSlide(<.div(
    <.h2("K Nearest Neighbours"),
    <.p(KNNWidget())
  ))
  .markdownSlides(
    """
    |## Decision boundaries
    |
    |Normally, KNN doesn't actually train a model - it just considers every new element based on all the data it's seen so far
    |
    |But we could decide to train a model from it by *imagining* some data points and seeing where they change classification --
    |the *decision boundary*
    |
    |If you consider the decision boundary as some kind of curve or line, again we're effectively modelling some kind of mathematical
    |function.
    |""".stripMargin)
  .veautifulSlide(<.div(
    <.h2("KNN Decision Boundary"),
    <.p(KNNBoundaryWidget())
  ))
  .markdownSlides(
    """
    |## Choosing k
    |
    |* Small k (e.g. k = 1) follows the training data very closely — if a stray data element has a different classification, everything close to it will too
    |* Large k smooths the boundary out, making it more robust to noise, but can perhaps blurring genuine differences between classes
    |* k is usually chosen by trial and error (e.g. cross-validation) - pragmatism in action again
    |* Usually k is odd, so you can't get a tie.
    |
    |
    |""".stripMargin)
  .veautifulSlide(<.div(
    markdown.div("""|## 1-NN and Voronoi diagrams
                    |
                    |When k = 1, the predicted class is just "whichever training point is closest". Which happens to be the same as a *Voronoi diagram*.
                    |
                    |Effectively, each data point claims a little bit of space around it as "like me"
                    |""".stripMargin),
    <.p(VoronoiWidget())
  ))
  .markdownSlides(
    """
    |
    |## K-Means Clustering
    |
    |For KNN, we had some pre-classified data to train a model with. We call that "supervised learning"
    |
    |Suppose we didn't, and we just wanted a model to take a look at some data, and cluster it into groups that are most similar. This is *K-means*
    |
    |The idea:
    |
    |1. Pick *k* (how many clusters we're looking for), and place *k* centroids — e.g. at random points in the data
    |2. **Assign**: give each point to its nearest centroid
    |3. **Update**: move each centroid to the mean of the points now assigned to it
    |4. Repeat steps 2 and 3 until the centroids stop moving (much) — the algorithm has *converged*
    |
    |However, you have to pick the number of clusters you want in advance.
    |
    |""".stripMargin)
  .veautifulSlide(<.div(
    <.h2("K-Means Clustering"),
    <.p(KMeansWidget())
  ))
  .markdownSlides(
    """
    |## Choosing k: the elbow method
    |
    |Since we have to pick k in advance, how do we pick a good one?
    |
    |* For each candidate k, run k-means and add up the *within-cluster sum of squares* (WCSS) — how far
    |  every point is from the centroid it's assigned to, squared and totalled
    |* WCSS always goes down as k increases (more clusters can only fit the data at least as well) —
    |  at the extreme, k = number of points gives WCSS = 0
    |* But past a point, adding more clusters buys you very little — the curve of WCSS against k bends and
    |  flattens out. That bend is the *elbow*, and it's a reasonable stopping point: enough clusters to
    |  capture the real structure, not so many that you're just carving up noise
    |
    |""".stripMargin)
  .veautifulSlide(<.div(
    <.h2("Choosing k with the Elbow Method"),
    <.p(ElbowWidget())
  ))
  .markdownSlides(
    """
    |## DBSCAN: clustering by density
    |
    |DBSCAN stands for Density-Based Spatial Clustering of Applications with Noise
    |
    |Basically, it's the idea that a "cluster" is a group of points that are surrounded by whitespace, so it's going
    |to try to find some using an algorithm. 
    |
    |Two parameters:
    |
    |* **eps** — how close two points need to be to count as neighbours
    |* **minPts** — how many neighbours (including itself) a point needs within eps to be a "core point"
    |
    |The idea:
    |
    |1. Any point with at least minPts neighbours within eps is a *core point* — clusters grow from these
    |2. Chain together core points that are each other's neighbours, plus the *border points* on their edges,
    |   into one cluster
    |3. Anything left over — too far from any dense region — is *noise*, not forced into a cluster
    |
    |""".stripMargin)
  .veautifulSlide(<.div(
    <.h2("DBSCAN"),
    <.p(DBSCANWidget())
  ))
  .markdownSlides(
    """
    |## The story so far -
    |
    |* Some of machine learning is about similarity to what we've seen before
    |
    |* If we have pre-classified examples to work from, it's "supervised learning". If we don't, it's "unsupervised learning"
    |
    |* In unsupervised learning, one of the problems is "what do we call the clusters"? They correspond to whatever combination
    |  of dimensions the algorithm happened to find most useful, not necessarily something easy to explain.
    |
    |* "Explainable AI" is then the question of "how did you come up with that?"
    |
    |""".stripMargin)
  .markdownSlide(willCcBy)
  .renderSlides
