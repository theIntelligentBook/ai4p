package ai4p.embeddings.widgets

import com.wbillingsley.veautiful.html.{<, Styling, DHtmlComponent, ^}
import org.scalajs.dom
import scala.concurrent.ExecutionContext.Implicits.global
import ai4p.{*, given}

/**
 * The classic "king - man + woman ≈ queen" vector arithmetic, on real pretrained GloVe vectors.
 * The "−" and "+" words are both optional - leave one (or both) blank to just add or subtract a
 * single word, e.g. "paris − france" alone, or "king" alone (which just finds king's neighbours).
 */
object WordAlgebraWidget {

  val styling = Styling(
    """|display: inline-block;
       |font-family: 'Lato', sans-serif;
       |max-width: 720px;
       |""".stripMargin
  ).modifiedBy(
    " .wa-panel" -> "margin-bottom: 18px; padding: 10px 14px; background: #f8f8f8; border: 1px solid #ddd; border-radius: 4px;",
    " .wa-title" -> "font-weight: bold; color: #5a074f; margin-bottom: 6px;",
    " .wa-row" -> "display: flex; align-items: center; gap: 8px; flex-wrap: wrap; margin: 6px 0;",
    " .wa-op" -> "font-size: 1.3rem; color: #888; font-family: monospace;",
    " input[type=text]" -> "font-family: monospace; font-size: 0.95rem; padding: 3px 6px; width: 110px; border: 1px solid #bbb; border-radius: 3px;",
    " .wa-result" -> "font-family: monospace; font-size: 0.9rem; margin-top: 8px;",
    " .wa-bar-row" -> "display: flex; align-items: center; gap: 6px; margin: 3px 0;",
    " .wa-bar-word" -> "width: 90px; font-family: monospace; font-size: 0.85rem; text-align: right;",
    " .wa-bar-track" -> "flex: 1; background: #eee; border-radius: 3px; height: 14px; max-width: 220px;",
    " .wa-bar-fill" -> "background: #3b82f6; height: 100%; border-radius: 3px;",
    " .wa-bar-val" -> "font-family: monospace; font-size: 0.8rem; color: #666; width: 40px;",
    " .wa-warn" -> "color: #991b1b; font-size: 0.85rem;",
    " .wa-hint" -> "color: #666; font-size: 0.8rem; margin-top: 4px;"
  ).register()

  private def similarityBar(word: String, sim: Double) =
    <.div(^.cls := "wa-bar-row",
      <.span(^.cls := "wa-bar-word", word),
      <.div(^.cls := "wa-bar-track",
        <.div(^.cls := "wa-bar-fill", ^.style := s"width: ${math.max(0, math.min(100, sim * 100))}%;")
      ),
      <.span(^.cls := "wa-bar-val", f"$sim%.2f")
    )

  private def textInput(value: String, onChange: String => Unit, placeholder: String = "") =
    <("input")(
      ^.attr("type") := "text", ^.attr("value") := value, ^.attr("placeholder") := placeholder,
      ^.on("change") ==> { (e: dom.Event) => onChange(e.target.asInstanceOf[dom.html.Input].value) }
    )
}

case class WordAlgebraWidget() extends DHtmlComponent {
  import WordAlgebraWidget._

  private var model: Option[EmbeddingModel] = None
  private var error: Option[String] = None

  private var posA: String = "king"
  private var negB: String = "man"
  private var posC: String = "woman"

  EmbeddingModel.load().foreach { m => model = Some(m); rerender() }
  EmbeddingModel.load().failed.foreach { e => error = Some(e.getMessage); rerender() }

  private def setA(w: String): Unit = { posA = w; rerender() }
  private def setB(w: String): Unit = { negB = w; rerender() }
  private def setC(w: String): Unit = { posC = w; rerender() }

  private def panel(m: EmbeddingModel) =
    val a = posA.trim
    val b = negB.trim
    val c = posC.trim

    val body =
      if a.isEmpty then <.div(^.cls := "wa-warn", "Type at least a starting word.")
      else
        val positive = if c.isEmpty then Seq(a) else Seq(a, c)
        val negative = if b.isEmpty then Seq.empty else Seq(b)
        m.analogy(positive, negative, n = 5) match
          case None => <.div(^.cls := "wa-warn", "One of those words isn't in this small vocabulary - try common words like king, man, woman, paris, france, germany...")
          case Some(results) => <.div(^.cls := "wa-result", results.map((w, s) => similarityBar(w, s)))

    <.div(^.cls := "wa-panel",
      <.div(^.cls := "wa-title", "Word algebra"),
      <.div(^.cls := "wa-row",
        textInput(posA, setA), <.span(^.cls := "wa-op", "−"),
        textInput(negB, setB, placeholder = "(optional)"), <.span(^.cls := "wa-op", "+"),
        textInput(posC, setC, placeholder = "(optional)"), <.span(^.cls := "wa-op", "=")
      ),
      body,
      <.div(^.cls := "wa-hint", "Try: paris − france + germany, or leave − and + blank to see king's own neighbours.")
    )

  override protected def render =
    val body = (model, error) match
      case (Some(m), _) => panel(m)
      case (None, Some(e)) => <.div(^.cls := "wa-warn", s"Couldn't load word vectors ($e)")
      case (None, None) => <.div(^.cls := "wa-hint", "Loading ~10,000 real word vectors...")
    <.div(^.cls := styling.className, body)
}
