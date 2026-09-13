package ai4p.embeddings.widgets

import com.wbillingsley.veautiful.html.{<, Styling, DHtmlComponent, VHtmlContent, ^}
import org.scalajs.dom
import ai4p.{*, given}

/** Turns the raw [[peterRabbit]] text into paragraphs ("lines"), and builds the same
  * "which lines does this word appear in" vectors as [[LineVectorModel]] - but over a real story,
  * so clicking a word shows the pattern somewhere more vivid than a handful of toy sentences. */
object PeterRabbitModel {

  /** Paragraphs, blank-line separated in the source text, de-wrapped onto one line each. */
  val lines: Vector[String] =
    peterRabbit.split("\n\\s*\n").map(_.split("\n").mkString(" ").trim).filter(_.nonEmpty).filterNot(_ == "THE END").toVector

  /** The display tokens (words, punctuation and all) making up each line, kept for rendering. */
  val tokens: Vector[Vector[String]] = lines.map(_.split("\\s+").toVector)

  /** A token's clickable key: just its letters, lower-cased - "McGregor's" and "McGregor!" both key to "mcgregors"/"mcgregor". */
  def keyOf(token: String): String = token.toLowerCase.filter(_.isLetter)

  private val keysPerLine: Vector[Set[String]] = tokens.map(_.map(keyOf).filter(_.nonEmpty).toSet)

  /** word-key -> one bit per line, 1 if that word appears in that line. */
  val vectors: Map[String, Vector[Int]] =
    val allKeys = keysPerLine.flatten.toSet
    allKeys.map(k => k -> keysPerLine.map(ks => if ks.contains(k) then 1 else 0)).toMap
}

object PeterRabbitWidget {
  import PeterRabbitModel._

  val styling = Styling(
    """|display: block;
       |max-width: 760px;
       |font-family: 'Lato', sans-serif;
       |""".stripMargin
  ).modifiedBy(
    " .pr-story" -> "max-height: 380px; overflow-y: auto; padding: 10px 14px; background: #fffdf7; border: 1px solid #e5dfce; border-radius: 4px; font-family: 'Playfair Display', serif; font-size: 1.05rem; line-height: 1.7;",
    " .pr-line" -> "margin: 0 0 10px 0; padding: 2px 4px; border-radius: 3px; transition: background 0.15s, opacity 0.15s;",
    " .pr-line.pr-hit" -> "background: #fff1a8;",
    " .pr-line.pr-dim" -> "opacity: 0.3;",
    " .pr-word" -> "cursor: pointer; border-radius: 2px;",
    " .pr-word:hover" -> "background: #eee;",
    " .pr-word.pr-selected" -> "background: #1d4ed8; color: white; font-weight: bold;",
    " .pr-info" -> "margin-top: 10px; font-family: monospace; font-size: 0.85rem; color: #333;",
    " .pr-vec" -> "display: flex; flex-wrap: wrap; gap: 2px; margin-top: 6px; max-width: 100%;",
    " .pr-bit" -> "width: 14px; height: 14px; border-radius: 2px; background: #eee;",
    " .pr-bit.pr-on" -> "background: #1d4ed8;",
    " .pr-hint" -> "color: #888; font-size: 0.85rem; margin-top: 8px;"
  ).register()

  private def renderToken(token: String, selected: Option[String]): VHtmlContent =
    val key = keyOf(token)
    if key.isEmpty then <.span(token + " ")
    else
      val isSelected = selected.contains(key)
      <.span(^.cls := s"pr-word${if isSelected then " pr-selected" else ""}",
        ^.attr("data-key") := key,
        token + " "
      )
}

case class PeterRabbitWidget() extends DHtmlComponent {
  import PeterRabbitModel._
  import PeterRabbitWidget._

  private var selected: Option[String] = Some("peter")

  private def select(key: String): Unit =
    selected = if selected.contains(key) then None else Some(key)
    rerender()

  private def storyLine(lineTokens: Vector[String], lineIdx: Int) =
    val hit = selected.exists(k => vectors.getOrElse(k, Vector.empty).lift(lineIdx).contains(1))
    val cls = selected match
      case None => "pr-line"
      case Some(_) if hit => "pr-line pr-hit"
      case Some(_) => "pr-line pr-dim"
    <.p(^.cls := cls,
      ^.on("click") ==> { (e: dom.Event) =>
        val key = e.target.asInstanceOf[dom.html.Element].getAttribute("data-key")
        if key != null && key.nonEmpty then select(key)
      },
      lineTokens.map(renderToken(_, selected))
    )

  private def vectorRow(key: String) =
    val v = vectors(key)
    <.div(
      <.div(^.cls := "pr-info",
        f"\"$key\" appears in ${v.sum} of ${v.size} paragraphs — vector = [${v.mkString(",")}]"
      ),
      <.div(^.cls := "pr-vec",
        v.map(bit => <.div(^.cls := s"pr-bit${if bit == 1 then " pr-on" else ""}"))
      )
    )

  override protected def render =
    val below = selected match
      case Some(key) => vectorRow(key)
      case None => <.div(^.cls := "pr-hint", "Click any word in the story to see which paragraphs it appears in.")
    <.div(^.cls := styling.className,
      <.div(^.cls := "pr-story",
        tokens.zipWithIndex.map((lineTokens, i) => storyLine(lineTokens, i))
      ),
      below
    )
}
