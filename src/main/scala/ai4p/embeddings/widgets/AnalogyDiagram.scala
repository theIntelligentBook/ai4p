package ai4p.embeddings.widgets

import com.wbillingsley.veautiful.html.{<, SVG, Styling, DSvgContent, VHtmlContent, ^}
import ai4p.{*, given}

/**
 * A fixed "parallelogram" picture of the classic word2vec analogy trick: real 50-d GloVe vectors
 * for a handful of words, projected onto two hand-picked directions (e.g. "gender" and "royalty")
 * so the offset between related pairs (man→king, woman→queen) visibly points the same way.
 *
 * The coordinates are precomputed (offline, from the same vectors [[EmbeddingModel]] loads) rather
 * than reduced live, since a clean 2-axis projection of just these words is easier to read than a
 * general-purpose PCA/t-SNE would be, and it keeps this diagram available even before/without the
 * larger vector file having loaded.
 */
object AnalogyDiagram {

  val styling = Styling(
    """|display: inline-block;
       |font-family: 'Lato', sans-serif;
       |""".stripMargin
  ).modifiedBy(
    " .ad-caption" -> "text-align: center; font-size: 0.85rem; color: #666; margin-top: 2px;",
    " .ad-group" -> "display: inline-block; vertical-align: top; margin-right: 18px;"
  ).register()

  case class Point(word: String, x: Double, y: Double, highlight: Boolean = true)
  case class Arrow(from: String, to: String, color: String)

  val genderRoyalty: Vector[Point] = Vector(
    Point("king", 3.0403, -1.4779), Point("queen", 1.7461, 0.5039),
    Point("man", -2.1035, -1.4779), Point("woman", -2.306, 1.1169),
    Point("prince", 2.0789, -0.9571, highlight = false), Point("princess", 1.3621, 1.3984, highlight = false),
    Point("boy", -1.7078, -0.2233, highlight = false), Point("girl", -2.1101, 1.117, highlight = false)
  )
  val genderRoyaltyArrows = Vector(
    Arrow("man", "king", "#3b82f6"), Arrow("woman", "queen", "#7c3aed")
  )

  val capitalCountry: Vector[Point] = Vector(
    Point("paris", 0.1469, 1.5364), Point("france", -0.0089, -1.3661),
    Point("berlin", 2.4721, 2.021), Point("germany", 2.5767, -1.5336),
    Point("rome", -0.5437, 1.5065, highlight = false), Point("italy", -0.7331, -1.2671, highlight = false),
    Point("madrid", -1.7121, 0.6366, highlight = false), Point("spain", -2.198, -1.5336, highlight = false)
  )
  val capitalCountryArrows = Vector(
    Arrow("france", "paris", "#3b82f6"), Arrow("germany", "berlin", "#7c3aed")
  )

  private def frame(w: Int, h: Int) = (x: Double, y: Double) =>
    val sx = w / 2.0 + x * (w / 2.0 - 40) / 3.2
    val sy = h / 2.0 - y * (h / 2.0 - 30) / 3.2
    (sx, sy)

  private def arrowHead(x1: Double, y1: Double, x2: Double, y2: Double, color: String): DSvgContent =
    val angle = math.atan2(y2 - y1, x2 - x1)
    val size = 7.0
    val a1 = angle + math.Pi - 0.4
    val a2 = angle + math.Pi + 0.4
    val p1x = x2 + size * math.cos(a1); val p1y = y2 + size * math.sin(a1)
    val p2x = x2 + size * math.cos(a2); val p2y = y2 + size * math.sin(a2)
    SVG.polygon(^.attr("points") := s"$x2,$y2 $p1x,$p1y $p2x,$p2y", ^.attr("fill") := color)

  def diagram(points: Vector[Point], arrows: Vector[Arrow], w: Int = 260, h: Int = 260): DSvgContent =
    val toSvg = frame(w, h)
    val byWord = points.map(p => p.word -> p).toMap
    SVG.svg(^.attr("width") := w, ^.attr("height") := h,
      SVG.g(
        for a <- arrows yield
          val (fromP, toP) = (byWord(a.from), byWord(a.to))
          val (x1, y1) = toSvg(fromP.x, fromP.y)
          val (x2, y2) = toSvg(toP.x, toP.y)
          SVG.g(
            SVG.line(^.attr("x1") := x1, ^.attr("y1") := y1, ^.attr("x2") := x2, ^.attr("y2") := y2,
              ^.attr("stroke") := a.color, ^.attr("stroke-width") := "2"),
            arrowHead(x1, y1, x2, y2, a.color)
          )
      ),
      SVG.g(
        for p <- points yield
          val (sx, sy) = toSvg(p.x, p.y)
          val r = if p.highlight then 4.5 else 3.0
          val fill = if p.highlight then "#1d4ed8" else "#bbb"
          SVG.g(
            SVG.circle(^.attr("cx") := sx, ^.attr("cy") := sy, ^.attr("r") := r, ^.attr("fill") := fill),
            SVG.text(^.attr("x") := sx + 6, ^.attr("y") := sy - 6,
              ^.attr("font-size") := (if p.highlight then "13" else "11"),
              ^.attr("fill") := (if p.highlight then "#222" else "#999"),
              ^.attr("font-family") := "monospace", p.word)
          )
      )
    )
}

/** The two mini analogy diagrams side by side, with captions. */
case class AnalogyDiagram() {
  import AnalogyDiagram._

  def render: VHtmlContent =
    <.div(^.cls := styling.className,
      <.div(^.cls := "ad-group",
        diagram(genderRoyalty, genderRoyaltyArrows),
        <.div(^.cls := "ad-caption", "man → king, woman → queen: same direction")
      ),
      <.div(^.cls := "ad-group",
        diagram(capitalCountry, capitalCountryArrows),
        <.div(^.cls := "ad-caption", "france → paris, germany → berlin: same direction")
      )
    )
}
