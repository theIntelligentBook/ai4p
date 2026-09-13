package ai4p.embeddings.widgets

import com.wbillingsley.veautiful.html.{<, Styling, DHtmlComponent, ^}
import org.scalajs.dom
import ai4p.{*, given}

/**
 * A silly, purely textual illustration of what stacking layers buys you: each layer is another
 * round of "mix in a bit of my relationship to every other token" - so here, literally, the phrase
 * "relationship between" gets repeated once per layer.
 */
object RelationshipRepeaterWidget {

  val styling = Styling(
    """
       |""".stripMargin
  ).modifiedBy(
    " .rr-controls" -> "display: flex; align-items: center; gap: 10px; margin-bottom: 14px;",
    " input[type=range]" -> "width: 220px;",
    " .rr-count" -> "font-family: monospace; font-weight: bold; color: #1d4ed8; width: 20px; text-align: center;",
    // " .rr-text" -> "font-size: 1.3rem; line-height: 1.6; max-width: 700px;"
  ).register()
}

case class RelationshipRepeaterWidget() extends DHtmlComponent {
  import RelationshipRepeaterWidget._

  private var layers: Int = 1

  private def setLayers(v: Int): Unit = { layers = math.max(1, math.min(96, v)); rerender() }

  override protected def render =
    val text = s"A network of $layers layers understands " + ("the relationship between " * layers) + "words."
    <.div(^.cls := styling.className,
      <.div(^.cls := "rr-controls",
        "layers:",
        <("input")(
          ^.attr("type") := "range", ^.attr("min") := "1", ^.attr("max") := "96", ^.attr("step") := "1",
          ^.attr("value") := layers.toString,
          ^.on("input") ==> { (e: dom.Event) => setLayers(e.target.asInstanceOf[dom.html.Input].value.toInt) }
        ),
        <.span(^.cls := "rr-count", layers.toString)
      ),
      <.div(^.cls := "rr-text", text)
    )
}
