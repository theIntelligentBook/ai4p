package ai4p.fromexamples.widgets

import com.wbillingsley.veautiful.html.{<, SVG, Styling, DHtmlComponent, ^}
import org.scalajs.dom

import scala.util.Random

import ai4p.{*, given}
import Common.*

import site.given

/** The CART-style tree-building machinery shared by the decision tree and random forest widgets. */
object DecisionTreeCore {

  val w = 500
  val h = 500
  val pad = 40

  val classColors = Vector("#3b82f6", "#ef4444")
  val classNames = Vector("Class A", "Class B")

  def toSvgX(x: Double): Double = pad + x * (w - 2 * pad)
  def toSvgY(y: Double): Double = (h - pad) - y * (h - 2 * pad)

  def toData(e: dom.MouseEvent): Option[(Double, Double)] =
    val rect = e.currentTarget.asInstanceOf[dom.svg.SVG].getBoundingClientRect()
    // rect is the on-screen (post CSS-transform) size, which may be scaled
    // relative to the SVG's own w x h coordinate space (e.g. inside a scaled slide deck)
    val svgX = (e.clientX - rect.left) * w / rect.width
    val svgY = (e.clientY - rect.top) * h / rect.height
    val px = (svgX - pad) / (w - 2 * pad)
    val py = 1.0 - (svgY - pad) / (h - 2 * pad)
    if px >= 0 && px <= 1 && py >= 0 && py <= 1 then Some((px, py)) else None

  /** A binary decision tree over two features (0 = x, 1 = y) and two classes. */
  enum Node:
    case Leaf(klass: Int, gini: Double, n: Int)
    case Branch(feature: Int, threshold: Double, gini: Double, n: Int, left: Node, right: Node)
  export Node.*

  def gini(labels: Seq[Int]): Double =
    if labels.isEmpty then 0.0
    else
      val n = labels.size.toDouble
      val counts = labels.groupBy(identity).view.mapValues(_.size).values
      1.0 - counts.map(c => math.pow(c / n, 2)).sum

  def majorityClass(labels: Seq[Int]): Int =
    labels.groupBy(identity).view.mapValues(_.size).toSeq.sortBy(-_._2).head._1

  /** Best (feature, threshold) split of `points`, searching only `features`, by lowest weighted Gini impurity */
  def bestSplit(points: Seq[(Double, Double, Int)], features: Seq[Int]): Option[(Int, Double, Double)] =
    val candidates = for
      f <- features
      values = points.map(p => if f == 0 then p._1 else p._2).distinct.sorted
      if values.size >= 2
      (a, b) <- values.zip(values.tail)
    yield (f, (a + b) / 2.0)

    if candidates.isEmpty then None
    else
      val n = points.size.toDouble
      val scored = candidates.map { case (f, t) =>
        val (leftPts, rightPts) = points.partition(p => (if f == 0 then p._1 else p._2) <= t)
        val wg = (leftPts.size * gini(leftPts.map(_._3)) + rightPts.size * gini(rightPts.map(_._3))) / n
        (f, t, wg)
      }
      Some(scored.minBy(_._3))

  /**
   * Recursively grows a tree by greedily splitting on whichever question most reduces Gini impurity.
   * `featureSubsetSize`, if given, restricts each split to a random subset of features of that size
   * (this is the "feature bagging" random forests use to decorrelate their trees).
   */
  def buildTree(
    points: Seq[(Double, Double, Int)],
    maxDepth: Int,
    minLeafSize: Int = 1,
    featureSubsetSize: Option[Int] = None,
    depth: Int = 0
  ): Node =
    val labels = points.map(_._3)
    val g = gini(labels)
    val majority = majorityClass(labels)
    val allFeatures = Seq(0, 1)
    val features = featureSubsetSize match
      case Some(k) if k < allFeatures.size => Random.shuffle(allFeatures).take(math.max(1, k))
      case _ => allFeatures

    if depth >= maxDepth || g <= 1e-9 || points.size < 2 * minLeafSize then
      Leaf(majority, g, points.size)
    else
      bestSplit(points, features) match
        case Some((f, t, wg)) if wg < g - 1e-9 =>
          val (leftPts, rightPts) = points.partition(p => (if f == 0 then p._1 else p._2) <= t)
          if leftPts.size < minLeafSize || rightPts.size < minLeafSize then
            Leaf(majority, g, points.size)
          else
            Branch(f, t, g, points.size,
              buildTree(leftPts, maxDepth, minLeafSize, featureSubsetSize, depth + 1),
              buildTree(rightPts, maxDepth, minLeafSize, featureSubsetSize, depth + 1)
            )
        case _ =>
          Leaf(majority, g, points.size)

  def predict(node: Node, x: Double, y: Double): Int =
    node match
      case Leaf(k, _, _) => k
      case Branch(f, t, _, _, l, r) =>
        val v = if f == 0 then x else y
        if v <= t then predict(l, x, y) else predict(r, x, y)

  def leafCount(node: Node): Int =
    node match
      case Leaf(_, _, _) => 1
      case Branch(_, _, _, _, l, r) => leafCount(l) + leafCount(r)

  def depthOf(node: Node): Int =
    node match
      case Leaf(_, _, _) => 0
      case Branch(_, _, _, _, l, r) => 1 + math.max(depthOf(l), depthOf(r))

  /** A monospaced, indented rendering of the tree's questions, in the style of the `tree` command */
  def renderLines(node: Node, featureNames: Vector[String] = Vector("x", "y")): Seq[String] =
    def go(node: Node, prefix: String, isTail: Boolean, isRoot: Boolean): Seq[String] =
      val connector = if isRoot then "" else if isTail then "└─ if no:  " else "├─ if yes: "
      node match
        case Leaf(k, g, n) =>
          Seq(prefix + connector + s"${classNames(k)}  (n=$n)")
        case Branch(f, t, g, n, l, r) =>
          val header = prefix + connector + f"${featureNames(f)} ≤ $t%.3f ?  (n=$n, gini=$g%.2f)"
          val childPrefix = prefix + (if isRoot then "" else if isTail then "    " else "│   ")
          header +: (go(l, childPrefix, false, false) ++ go(r, childPrefix, true, false))
    go(node, "", isTail = true, isRoot = true)

  def bootstrapSample(points: Seq[(Double, Double, Int)]): Seq[(Double, Double, Int)] =
    if points.isEmpty then points
    else Vector.fill(points.size)(points(Random.nextInt(points.size)))

  /** Roughly gaussian jitter in about [-spread, spread], via the sum of a few uniforms */
  def jitter(spread: Double): Double =
    (Random.nextDouble() + Random.nextDouble() + Random.nextDouble() - 1.5) / 1.5 * spread

  def defaultPoints: Seq[(Double, Double, Int)] = Seq(
    (0.15, 0.20, 0), (0.20, 0.35, 0), (0.10, 0.45, 0), (0.28, 0.15, 0), (0.35, 0.40, 0),
    (0.55, 0.62, 0),
    (0.75, 0.70, 1), (0.80, 0.55, 1), (0.65, 0.80, 1), (0.85, 0.75, 1), (0.70, 0.88, 1),
    (0.28, 0.78, 1),
    (0.60, 0.25, 1), (0.55, 0.15, 1), (0.45, 0.30, 1)
  )

  /** Two fresh random blobs, one per class, each with a stray outlier planted in the other's territory */
  def randomBlobs(): Seq[(Double, Double, Int)] =
    val c0 = (0.2 + Random.nextDouble() * 0.2, 0.2 + Random.nextDouble() * 0.2)
    val c1 = (0.6 + Random.nextDouble() * 0.2, 0.6 + Random.nextDouble() * 0.2)
    def clamp(v: Double): Double = math.max(0.02, math.min(0.98, v))
    val a = (0 until 7).map(_ => (clamp(c0._1 + jitter(0.15)), clamp(c0._2 + jitter(0.15)), 0))
    val b = (0 until 7).map(_ => (clamp(c1._1 + jitter(0.15)), clamp(c1._2 + jitter(0.15)), 1))
    val outlier0 = (clamp(c1._1 + jitter(0.06)), clamp(c1._2 + jitter(0.06)), 0)
    val outlier1 = (clamp(c0._1 + jitter(0.06)), clamp(c0._2 + jitter(0.06)), 1)
    (a ++ b :+ outlier0 :+ outlier1)

}

object DecisionTreeWidget {
  val styling = Styling(
    """|display: inline-block;
       |font-family: 'Lato', sans-serif;
       |""".stripMargin
  ).modifiedBy(
    " svg"           -> "background: #f8f8f8; border: 1px solid #ddd; border-radius: 4px; cursor: crosshair;",
    " .dt-controls"  -> "margin-top: 8px; display: flex; gap: 8px; align-items: center; flex-wrap: wrap;",
    " .dt-info"      -> "font-family: 'Playfair Design', serif; font-style: italic; font-size: 1.05rem; color: #5a074f; margin-top: 6px;",
    " .dt-legend"    -> "font-size: 0.8rem; color: #666; margin-top: 4px; display: flex; gap: 14px; flex-wrap: wrap;",
    " .dt-legend-item" -> "display: inline-flex; align-items: center;",
    " .dt-swatch"    -> "display: inline-block; width: 10px; height: 10px; border-radius: 50%; margin-right: 5px;",
    " .dt-label"     -> "font-size: 0.85rem; color: #666;",
    " input[type=range]" -> "width: 120px;",
    " .dt-tree"      -> "margin-top: 10px; background: #1e1e1e; color: #d4d4d4; padding: 10px 14px; border-radius: 6px; font-family: 'Menlo','Consolas',monospace; font-size: 0.72rem; line-height: 1.5; white-space: pre; max-height: 260px; overflow: auto;"
  ).register()
}

case class DecisionTreeWidget(initialMaxDepth: Int = 3) extends DHtmlComponent {

  import DecisionTreeWidget._
  import DecisionTreeCore._

  var points: Seq[(Double, Double, Int)] = defaultPoints
  var activeClass: Int = 0
  var maxDepth: Int = initialMaxDepth
  val resolution: Int = 45

  def handleClick(e: dom.MouseEvent): Unit =
    toData(e).foreach { p => points = points :+ (p._1, p._2, activeClass) }
    rerender()

  override protected def render =
    val tree = if points.nonEmpty then Some(buildTree(points, maxDepth)) else None
    val cellSize = (w - 2 * pad).toDouble / resolution

    <.div(^.cls := styling.className,
      SVG.svg(
        ^.attr("width") := w, ^.attr("height") := h,
        ^.onClick ==> handleClick,

        tree match
          case Some(t) =>
            for
              gx <- 0 until resolution
              gy <- 0 until resolution
            yield
              val cx = (gx + 0.5) / resolution
              val cy = (gy + 0.5) / resolution
              val predicted = predict(t, cx, cy)
              SVG.rect(
                ^.attr("x") := toSvgX(cx) - cellSize / 2,
                ^.attr("y") := toSvgY(cy) - cellSize / 2,
                ^.attr("width") := cellSize + 0.5,
                ^.attr("height") := cellSize + 0.5,
                ^.attr("fill") := classColors(predicted),
                ^.attr("fill-opacity") := "0.30",
                ^.attr("stroke") := "none"
              )
          case None => SVG.g(),

        SVG.rect(^.attr("x") := pad, ^.attr("y") := pad,
          ^.attr("width") := w - 2 * pad, ^.attr("height") := h - 2 * pad,
          ^.attr("fill") := "none", ^.attr("stroke") := "#ccc"),

        for (x, y, c) <- points yield
          SVG.circle(
            ^.attr("cx") := toSvgX(x), ^.attr("cy") := toSvgY(y),
            ^.attr("r") := "6",
            ^.attr("fill") := classColors(c),
            ^.attr("stroke") := "white", ^.attr("stroke-width") := "1.5"
          )
      ),

      <.div(^.cls := "dt-info",
        tree match
          case Some(t) => s"max depth = $maxDepth  →  ${leafCount(t)} leaves (actual depth ${depthOf(t)})"
          case None => "Click the chart to add labelled points"
      ),

      <.div(^.cls := "dt-legend",
        for (name, i) <- classNames.zipWithIndex yield
          <.span(^.cls := "dt-legend-item",
            <.span(^.cls := "dt-swatch", ^.style := s"background: ${classColors(i)};"), name)
      ),

      <.div(^.cls := "dt-controls",
        <.span("Place:"),
        for (name, i) <- classNames.zipWithIndex yield
          <.button(
            ^.cls := (if activeClass == i then "btn btn-primary btn-sm" else "btn btn-outline-primary btn-sm"),
            ^.onClick --> { activeClass = i; rerender() },
            name
          ),
        <.button(^.cls := "btn btn-outline-secondary btn-sm",
          ^.onClick --> { points = randomBlobs(); rerender() },
          "New points"
        ),
        <.button(^.cls := "btn btn-outline-secondary btn-sm",
          ^.onClick --> { points = Seq.empty; rerender() },
          "Clear points"
        ),

        <.span(^.cls := "dt-label", "max depth ="),
        <("input")(
          ^.attr("type") := "range",
          ^.attr("min") := "1", ^.attr("max") := "8", ^.attr("step") := "1",
          ^.attr("value") := maxDepth.toString,
          ^.on("input") ==> { (e: dom.Event) =>
            maxDepth = e.target.asInstanceOf[dom.html.Input].value.toInt
            rerender()
          }
        ),
        <.span(^.cls := "dt-label", maxDepth.toString)
      ),

      tree match
        case Some(t) => <.div(^.cls := "dt-tree", renderLines(t).mkString("\n"))
        case None => <.span()
    )
}

object RandomForestWidget {
  val styling = Styling(
    """|display: inline-block;
       |font-family: 'Lato', sans-serif;
       |""".stripMargin
  ).modifiedBy(
    " svg"             -> "background: #f8f8f8; border: 1px solid #ddd; border-radius: 4px; cursor: crosshair;",
    " .rf-row"         -> "display: flex; gap: 16px; align-items: flex-start; flex-wrap: wrap;",
    " .rf-panel"       -> "display: flex; flex-direction: column; align-items: center;",
    " .rf-panel-title" -> "font-size: 0.8rem; color: #666; margin-bottom: 4px;",
    " .rf-controls"    -> "margin-top: 8px; display: flex; gap: 8px; align-items: center; flex-wrap: wrap;",
    " .rf-info"        -> "font-family: 'Playfair Design', serif; font-style: italic; font-size: 1.05rem; color: #5a074f; margin-top: 6px;",
    " .rf-legend"      -> "font-size: 0.8rem; color: #666; margin-top: 4px; display: flex; gap: 14px; flex-wrap: wrap;",
    " .rf-legend-item" -> "display: inline-flex; align-items: center;",
    " .rf-swatch"      -> "display: inline-block; width: 10px; height: 10px; border-radius: 50%; margin-right: 5px;",
    " .rf-label"       -> "font-size: 0.85rem; color: #666;",
    " input[type=range]" -> "width: 120px;"
  ).register()
}

case class RandomForestWidget(initialTrees: Int = 15, initialMaxDepth: Int = 3) extends DHtmlComponent {

  import RandomForestWidget._
  import DecisionTreeCore._

  val panelW = 380
  val panelH = 380
  val panelPad = 30
  val resolution = 40

  var points: Seq[(Double, Double, Int)] = defaultPoints
  var activeClass: Int = 0
  var numTrees: Int = initialTrees
  var maxDepth: Int = initialMaxDepth

  def panelX(x: Double): Double = panelPad + x * (panelW - 2 * panelPad)
  def panelY(y: Double): Double = (panelH - panelPad) - y * (panelH - 2 * panelPad)

  def handleClick(e: dom.MouseEvent): Unit =
    toData(e).foreach { p => points = points :+ (p._1, p._2, activeClass) }
    rerender()

  def boundaryCells(predictAt: (Double, Double) => Int) =
    val cellSize = (panelW - 2 * panelPad).toDouble / resolution
    for
      gx <- 0 until resolution
      gy <- 0 until resolution
    yield
      val cx = (gx + 0.5) / resolution
      val cy = (gy + 0.5) / resolution
      SVG.rect(
        ^.attr("x") := panelX(cx) - cellSize / 2,
        ^.attr("y") := panelY(cy) - cellSize / 2,
        ^.attr("width") := cellSize + 0.5,
        ^.attr("height") := cellSize + 0.5,
        ^.attr("fill") := classColors(predictAt(cx, cy)),
        ^.attr("fill-opacity") := "0.30",
        ^.attr("stroke") := "none"
      )

  def pointDots =
    for (x, y, c) <- points yield
      SVG.circle(
        ^.attr("cx") := panelX(x), ^.attr("cy") := panelY(y),
        ^.attr("r") := "5",
        ^.attr("fill") := classColors(c),
        ^.attr("stroke") := "white", ^.attr("stroke-width") := "1.5"
      )

  override protected def render =
    val hasPoints = points.nonEmpty

    val singleTree = if hasPoints then Some(buildTree(points, maxDepth)) else None
    // A fresh bootstrap sample and a random single-feature restriction per tree, per render —
    // so "Regrow forest" (or any control change) shows a newly-sampled forest
    val forestTrees =
      if hasPoints then (0 until numTrees).map(_ => buildTree(bootstrapSample(points), maxDepth, featureSubsetSize = Some(1)))
      else Seq.empty

    def singlePredict(x: Double, y: Double): Int = singleTree.map(predict(_, x, y)).getOrElse(0)
    def forestPredict(x: Double, y: Double): Int =
      if forestTrees.isEmpty then 0
      else majorityClass(forestTrees.map(t => predict(t, x, y)))

    <.div(^.cls := styling.className,
      <.div(^.cls := "rf-row",

        <.div(^.cls := "rf-panel",
          <.div(^.cls := "rf-panel-title", "One decision tree"),
          SVG.svg(
            ^.attr("width") := panelW, ^.attr("height") := panelH,
            ^.onClick ==> handleClick,
            if hasPoints then boundaryCells(singlePredict) else SVG.g(),
            SVG.rect(^.attr("x") := panelPad, ^.attr("y") := panelPad,
              ^.attr("width") := panelW - 2 * panelPad, ^.attr("height") := panelH - 2 * panelPad,
              ^.attr("fill") := "none", ^.attr("stroke") := "#ccc"),
            pointDots
          )
        ),

        <.div(^.cls := "rf-panel",
          <.div(^.cls := "rf-panel-title", s"Forest of $numTrees trees (majority vote)"),
          SVG.svg(
            ^.attr("width") := panelW, ^.attr("height") := panelH,
            ^.onClick ==> handleClick,
            if hasPoints then boundaryCells(forestPredict) else SVG.g(),
            SVG.rect(^.attr("x") := panelPad, ^.attr("y") := panelPad,
              ^.attr("width") := panelW - 2 * panelPad, ^.attr("height") := panelH - 2 * panelPad,
              ^.attr("fill") := "none", ^.attr("stroke") := "#ccc"),
            pointDots
          )
        )
      ),

      <.div(^.cls := "rf-info",
        if hasPoints then
          s"Each tree: a bootstrap sample of the data, considering only 1 of 2 features per split — depth $maxDepth"
        else "Click either chart to add labelled points"
      ),

      <.div(^.cls := "rf-legend",
        for (name, i) <- classNames.zipWithIndex yield
          <.span(^.cls := "rf-legend-item",
            <.span(^.cls := "rf-swatch", ^.style := s"background: ${classColors(i)};"), name)
      ),

      <.div(^.cls := "rf-controls",
        <.span("Place:"),
        for (name, i) <- classNames.zipWithIndex yield
          <.button(
            ^.cls := (if activeClass == i then "btn btn-primary btn-sm" else "btn btn-outline-primary btn-sm"),
            ^.onClick --> { activeClass = i; rerender() },
            name
          ),
        <.button(^.cls := "btn btn-outline-secondary btn-sm",
          ^.onClick --> { points = randomBlobs(); rerender() },
          "New points"
        ),
        <.button(^.cls := "btn btn-outline-secondary btn-sm",
          ^.onClick --> { points = Seq.empty; rerender() },
          "Clear points"
        ),
        <.button(^.cls := "btn btn-outline-warning btn-sm",
          ^.onClick --> rerender(),
          "Regrow forest"
        ),

        <.span(^.cls := "rf-label", "trees ="),
        <("input")(
          ^.attr("type") := "range",
          ^.attr("min") := "1", ^.attr("max") := "40", ^.attr("step") := "1",
          ^.attr("value") := numTrees.toString,
          ^.on("input") ==> { (e: dom.Event) =>
            numTrees = e.target.asInstanceOf[dom.html.Input].value.toInt
            rerender()
          }
        ),
        <.span(^.cls := "rf-label", numTrees.toString),

        <.span(^.cls := "rf-label", "depth ="),
        <("input")(
          ^.attr("type") := "range",
          ^.attr("min") := "1", ^.attr("max") := "8", ^.attr("step") := "1",
          ^.attr("value") := maxDepth.toString,
          ^.on("input") ==> { (e: dom.Event) =>
            maxDepth = e.target.asInstanceOf[dom.html.Input].value.toInt
            rerender()
          }
        ),
        <.span(^.cls := "rf-label", maxDepth.toString)
      )
    )
}
