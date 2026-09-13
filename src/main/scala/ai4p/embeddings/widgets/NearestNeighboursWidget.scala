package ai4p.embeddings.widgets

import com.wbillingsley.veautiful.html.{<, SVG, Styling, DHtmlComponent, ^}
import org.scalajs.dom
import scala.concurrent.ExecutionContext.Implicits.global
import ai4p.{*, given}
import ai4p.neuralnets.widgets.NetworkView

/**
 * Lets you type a word and see its real, pretrained 50-d GloVe vector (trimmed to the ~10,000
 * most common words) - both the literal numbers and its nearest neighbours by cosine similarity.
 *
 * Nothing is trained here - the vectors are just loaded from a static file - so this is about
 * showing what a *good* embedding gives you, ready-made, rather than how one gets learned.
 */
object NearestNeighboursWidget {

  val styling = Styling(
    """|display: inline-block;
       |font-family: 'Lato', sans-serif;
       |max-width: 720px;
       |""".stripMargin
  ).modifiedBy(
    " .wv-panel" -> "margin-bottom: 18px; padding: 10px 14px; background: #f8f8f8; border: 1px solid #ddd; border-radius: 4px;",
    " .wv-title" -> "font-weight: bold; color: #5a074f; margin-bottom: 6px;",
    " .wv-row" -> "display: flex; align-items: center; gap: 8px; flex-wrap: wrap; margin: 6px 0;",
    " input[type=text]" -> "font-family: monospace; font-size: 0.95rem; padding: 3px 6px; width: 110px; border: 1px solid #bbb; border-radius: 3px;",
    " .wv-result" -> "font-family: monospace; font-size: 0.9rem; margin-top: 8px;",
    " .wv-bar-row" -> "display: flex; align-items: center; gap: 6px; margin: 3px 0;",
    " .wv-bar-word" -> "width: 90px; font-family: monospace; font-size: 0.85rem; text-align: right;",
    " .wv-bar-track" -> "flex: 1; background: #eee; border-radius: 3px; height: 14px; max-width: 220px;",
    " .wv-bar-fill" -> "background: #3b82f6; height: 100%; border-radius: 3px;",
    " .wv-bar-val" -> "font-family: monospace; font-size: 0.8rem; color: #666; width: 40px;",
    " .wv-warn" -> "color: #991b1b; font-size: 0.85rem;",
    " .wv-hint" -> "color: #666; font-size: 0.8rem; margin-top: 4px;",
    " .wv-vector" -> "margin: 8px 0;",
    " .wv-vector-label" -> "font-size: 0.8rem; color: #666; margin-bottom: 3px;",
    " .wv-vector-nums" -> "font-family: monospace; font-size: 0.7rem; color: #666; margin-top: 4px; word-break: break-all;"
  ).register()

  /** The word's 50 numbers as a compact strip - blue for positive, orange for negative, darker
    * the further from zero (relative to this word's own biggest entry) - plus the literal numbers
    * underneath, since a vector being "just numbers" is the whole point. */
  private def vectorSwatch(word: String, v: Array[Double]) =
    val scale = math.max(1e-9, v.map(math.abs).max)
    <.div(^.cls := "wv-vector",
      <.div(^.cls := "wv-vector-label", s"\"$word\" as a vector (${v.length} numbers):"),
      SVG.svg(^.attr("width") := v.length * 7, ^.attr("height") := 24,
        SVG.g(
          v.zipWithIndex.map((x, i) =>
            SVG.rect(^.attr("x") := i * 7, ^.attr("y") := 0, ^.attr("width") := 6, ^.attr("height") := 24,
              ^.attr("fill") := NetworkView.divergingColor(x / scale))
          )
        )
      ),
      <.div(^.cls := "wv-vector-nums", v.map(x => f"$x%.2f").mkString(", "))
    )

  private def similarityBar(word: String, sim: Double) =
    <.div(^.cls := "wv-bar-row",
      <.span(^.cls := "wv-bar-word", word),
      <.div(^.cls := "wv-bar-track",
        <.div(^.cls := "wv-bar-fill", ^.style := s"width: ${math.max(0, math.min(100, sim * 100))}%;")
      ),
      <.span(^.cls := "wv-bar-val", f"$sim%.2f")
    )

  private def textInput(value: String, onChange: String => Unit) =
    <("input")(
      ^.attr("type") := "text", ^.attr("value") := value,
      ^.on("change") ==> { (e: dom.Event) => onChange(e.target.asInstanceOf[dom.html.Input].value) }
    )
}

case class NearestNeighboursWidget() extends DHtmlComponent {
  import NearestNeighboursWidget._

  private var model: Option[EmbeddingModel] = None
  private var error: Option[String] = None

  private var neighbourWord: String = "computer"

  EmbeddingModel.load().foreach { m => model = Some(m); rerender() }
  EmbeddingModel.load().failed.foreach { e => error = Some(e.getMessage); rerender() }

  private def setNeighbourWord(w: String): Unit = { neighbourWord = w; rerender() }

  private def panel(m: EmbeddingModel) =
    val body = m.vector(neighbourWord) match
      case None => <.div(^.cls := "wv-warn", s"\"$neighbourWord\" isn't in this small vocabulary - try a common word.")
      case Some(v) =>
        <.div(
          vectorSwatch(neighbourWord, v),
          <.div(^.cls := "wv-result",
            m.nearest(v, n = 8, exclude = Set(neighbourWord.trim.toLowerCase)).map((w, s) => similarityBar(w, s))
          )
        )
    <.div(^.cls := "wv-panel",
      <.div(^.cls := "wv-title", "Nearest neighbours"),
      <.div(^.cls := "wv-row",
        "word =", textInput(neighbourWord, setNeighbourWord)
      ),
      body
    )

  override protected def render =
    val body = (model, error) match
      case (Some(m), _) => panel(m)
      case (None, Some(e)) => <.div(^.cls := "wv-warn", s"Couldn't load word vectors ($e)")
      case (None, None) => <.div(^.cls := "wv-hint", "Loading ~10,000 real word vectors...")
    <.div(^.cls := styling.className, body)
}
