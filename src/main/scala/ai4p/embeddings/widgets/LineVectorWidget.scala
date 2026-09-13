package ai4p.embeddings.widgets

import com.wbillingsley.veautiful.html.{<, Styling, DHtmlComponent, VHtmlContent, ^}
import com.wbillingsley.veautiful.MutableArrayComponent.*
import org.scalajs.dom
import ai4p.{*, given}

/**
 * The naive "represent a word by which lines it appears in" idea, made concrete on the same
 * Peter Rabbit text as [[PeterRabbitWidget]]: each word becomes a vector of 0s and 1s, one bit
 * per paragraph. Click any word to make it the reference, and every other word gets shaded and
 * ranked by how similar it is - the point being that words used in similar ways (e.g. "cat" and
 * "mouse", both animals Peter meets alone) can still end up with completely unrelated vectors,
 * just because they never land in the same paragraph. Add that this vector's length is the
 * number of paragraphs in the corpus (36 here, millions for a real one) and it's easy to see why
 * this doesn't scale.
 *
 * With ~295 words x 36 paragraphs, the grid is a few thousand cells - too many to rebuild and
 * re-diff as ordinary declarative content every time the reference word changes (that was the
 * slow part). Instead the grid is built *once* with `generateChildren`, d3-style, one managed
 * item per *row* (not per cell - a first attempt at one item per cell was still slow, since even
 * a no-op `onUpdate` call has a real per-item cost, and ~11,000 of them adds up). Each row is a
 * `display: contents` wrapper so its own children (the label, the 36 bit-cells and the similarity
 * cell) still lay out as if they were direct children of the CSS grid. Picking a new reference
 * word calls `grid.update()`, which reaches straight into ~296 row DOM nodes and recolours their
 * insides - no virtual-dom diffing of the grid at all.
 */
object LineVectorWidget {
  import LineVectorModel._

  val styling = Styling(
    """|display: inline-block;
       |font-family: 'Lato', sans-serif;
       |""".stripMargin
  ).modifiedBy(
    " .lv-grid-scroll" -> "overflow-x: auto; max-width: 100%; max-height: 380px; overflow-y: scroll;",
    " .lv-grid" -> "display: grid; gap: 2px; align-items: center; font-size: 0.65rem;",
    " .lv-corner" -> "font-size: 0.6rem; color: #999;",
    " .lv-colhead" -> "text-align: center; color: #999; font-family: monospace; font-size: 0.55rem;",
    " .lv-rowhead" -> "font-family: monospace; text-align: right; padding-right: 4px; cursor: pointer; white-space: nowrap; border-radius: 2px;",
    " .lv-cell" -> "width: 16px; height: 16px; border-radius: 2px; background: #eee;",
    " .lv-cell.lv-on" -> "background: #3b82f6;",
    " .lv-simcell" -> "font-family: monospace; text-align: right; padding-left: 6px; color: #333;",
    " .lv-controls" -> "margin-top: 12px; display: flex; align-items: center; gap: 8px; flex-wrap: wrap;",
    " select" -> "font-size: 0.85rem; padding: 2px 4px;",
    " .lv-legend" -> "margin-top: 6px; font-size: 0.8rem; color: #666;",
    " .lv-related" -> "margin-top: 10px; font-size: 20px;",
    " .lv-related-title" -> "font-weight: bold; margin-bottom: 4px;",
    " .lv-related-row" -> "display: flex; align-items: center; gap: 6px; margin: 2px 0;",
    " .lv-related-word" -> "width: 180px; font-family: monospace; text-align: right;",
    " .lv-related-track" -> "flex: 1; background: #eee; border-radius: 3px; height: 12px;",
    " .lv-related-fill" -> "background: #16a34a; height: 100%; border-radius: 3px;",
    " .lv-related-val" -> "font-family: monospace; font-size: 0.8rem; color: #666; width: 36px;",
    " .lv-scale" -> "margin-top: 10px; font-size: 0.85rem; color: #666; max-width: 480px;"
  ).register()

  private def wordSelect(value: String, onChange: String => Unit) =
    <.select(
      ^.on("change") ==> { (e: dom.Event) => onChange(e.target.asInstanceOf[dom.html.Select].value) },
      for w <- vocabulary yield
        <.option(^.attr("value") := w, ^.attr("selected") ?= (if w == value then Some("selected") else None), w)
    )

  /** White (unrelated) through to a saturated green (identical rows), for shading a word by its
    * similarity to the currently-selected reference word. */
  private def greenShade(sim: Double): String =
    val t = math.max(0.0, math.min(1.0, sim))
    val r = math.round(255 - t * (255 - 22))
    val g = math.round(255 - t * (255 - 163))
    val b = math.round(255 - t * (255 - 74))
    s"rgb($r,$g,$b)"

  private def textColorFor(sim: Double): String = if sim > 0.55 then "white" else "#222"

  /** One direct-ish (via `display: contents`) contributor to the `.lv-grid` container - either
    * the fixed header row, or one word's row. */
  private enum Row:
    case Header
    case Word(w: String)

  private val rowData: Array[Row] = (Row.Header +: vocabulary.map(Row.Word(_))).toArray
}

case class LineVectorWidget() extends DHtmlComponent {
  import LineVectorModel._
  import LineVectorWidget._

  private var wordA: String = "peter"
  private var prevWordA: String = wordA

  private def setA(w: String): Unit =
    prevWordA = wordA
    wordA = w
    grid.update()
    rerender()

  /** The other curated words, ranked by similarity to the reference word. */
  private def topRelated(n: Int = 5): Vector[(String, Double)] =
    val vecA = vectors(wordA)
    vocabulary.filterNot(_ == wordA)
      .map(w => w -> cosine(vectors(w), vecA))
      .sortBy(-_._2)
      .take(n)

  private def paintLabel(el: dom.html.Element, w: String): Unit =
    if w == wordA then
      el.style.background = "#dbeafe"
      el.style.color = "#1d4ed8"
      el.style.fontWeight = "bold"
    else
      val sim = cosine(vectors(w), vectors(wordA))
      el.style.background = greenShade(sim)
      el.style.color = textColorFor(sim)
      el.style.fontWeight = "normal"

  private def paintSim(el: dom.Node, w: String): Unit =
    el.textContent = if w == wordA then "—" else f"${cosine(vectors(w), vectors(wordA))}%.2f"

  // A bit-cell's *default* colour (on/off) comes from the "lv-on" CSS class set once when it's
  // created - that never changes, so it's already correct from the moment the row is built.
  // Only the "this row is the current reference word" highlight is imperative, and only ever
  // needs applying to the new reference row, and clearing (reverting to the CSS default) from
  // the previous one - not touching the other ~295 rows' cells at all.
  private def highlightBits(kids: dom.HTMLCollection[dom.Element], w: String): Unit =
    val bits = vectors(w)
    var col = 0
    while col < bits.length do
      if bits(col) == 1 then kids(1 + col).asInstanceOf[dom.html.Element].style.background = "#1d4ed8"
      col += 1

  private def clearBitHighlight(kids: dom.HTMLCollection[dom.Element]): Unit =
    var i = 1
    while i < kids.length - 1 do
      kids(i).asInstanceOf[dom.html.Element].style.background = ""
      i += 1

  // Built once, one managed item per *row* (not per cell - see the class doc). `onEnter` lays out
  // a row's fixed shape (label, 36 bit-cells, similarity cell, all as `display: contents` grid
  // children); `onUpdate` reaches into that one row's already-built DOM children by position to
  // repaint whatever depends on the current reference word.
  private val grid = <.div(^.cls := "lv-grid",
      ^.style := s"grid-template-columns: 90px repeat(${corpus.size}, 16px) 40px;"
    ).generateChildren(rowData) {
      case (Row.Header, _) =>
        <.div(^.style := "display: contents",
          <.div(^.cls := "lv-corner", "word \\ line"),
          corpus.indices.map(i => <.div(^.cls := "lv-colhead", (i + 1).toString)),
          <.div(^.cls := "lv-colhead", "sim")
        )
      case (Row.Word(w), _) =>
        <.div(^.style := "display: contents",
          <.div(^.cls := "lv-rowhead", ^.onClick --> setA(w), w),
          vectors(w).map(bit => <.div(^.cls := s"lv-cell${if bit == 1 then " lv-on" else ""}")),
          <.div(^.cls := "lv-simcell")
        )
    }.onUpdate {
      case (Row.Header, _, _) => () // static - never changes
      case (Row.Word(w), _, v) =>
        for wrapper <- v.domNode do
          val kids = wrapper.children
          paintLabel(kids(0).asInstanceOf[dom.html.Element], w)
          paintSim(kids(kids.length - 1), w)
          if w == wordA then highlightBits(kids, w)
          else if w == prevWordA then clearBitHighlight(kids)
    }

  override def afterAttach(): Unit = grid.update()

  private def relatedList =
    <.div(^.cls := "lv-related",
      <.div(^.cls := "lv-related-title", s"Most related to \"$wordA\":"),
      topRelated().map((w, sim) =>
        <.div(^.cls := "lv-related-row", ^.key := w,
          <.span(^.cls := "lv-related-word", w),
          <.div(^.cls := "lv-related-track",
            <.div(^.cls := "lv-related-fill", ^.style := s"width: ${math.max(0, math.min(100, sim * 100))}%;")
          ),
          <.span(^.cls := "lv-related-val", f"$sim%.2f")
        )
      )
    )

  override protected def render =
    <.div(^.cls := styling.className,
      <.div(^.cls := "lv-grid-scroll", grid),
      <.div(^.cls := "lv-legend",
        s"Click any word to make it the reference (left, blue). Every other row's shading and " +
        s"right-hand column show its similarity to \"$wordA\"."
      ),
      <.div(^.cls := "lv-controls",
        "reference word:", wordSelect(wordA, setA)
      ),
      relatedList,
    )
}
