package ai4p.doingstuff.widgets

import com.wbillingsley.veautiful.html.{<, Styling, DHtmlComponent, ^}
import org.scalajs.dom
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Success, Failure}
import ai4p.{*, given}

/**
 * Ties [[EtchASketchWidget]] (the "server's" tool) and [[LlmTrafficWidget]] (the traffic log)
 * together with a real call to an LLM via [[Puter]]. There's no actual MCP server or client here -
 * it's all one browser tab - but the interesting part of MCP's tool-calling loop (hand the model a
 * tool list; let it decide what to call, in what order, with what arguments) is exactly what
 * happens when you type something and click the button below.
 */
object McpDrawDemo {

  val styling = Styling(
    """|display: flex;
       |gap: 28px;
       |flex-wrap: wrap;
       |align-items: flex-start;
       |font-family: 'Lato', sans-serif;
       |""".stripMargin
  ).modifiedBy(
    " .md-ask-row" -> "display: flex; gap: 6px; align-items: center; margin-bottom: 8px; flex-wrap: wrap;",
    " .md-presets" -> "margin-bottom: 10px;",
    " .md-presets button" -> "margin-right: 4px;",
    " input[type=text]" -> "font-family: 'Lato', sans-serif; font-size: 0.9rem; padding: 4px 8px; width: 200px; border: 1px solid #bbb; border-radius: 3px;"
  ).register()

  private def textInput(value: String, onChange: String => Unit) =
    <("input")(
      ^.attr("type") := "text", ^.attr("value") := value,
      ^.on("change") ==> { (e: dom.Event) => onChange(e.target.asInstanceOf[dom.html.Input].value) }
    )
}

case class McpDrawDemo() extends DHtmlComponent {
  import McpDrawDemo._
  import EtchASketchModel.Command

  private var sketch: EtchASketchModel = EtchASketchModel.empty
  private var request: String = "a house"
  private var sentPrompt: Option[String] = None
  private var rawResponse: Option[String] = None
  private var status: String = "Type something to draw, or pick a preset, then click \"Ask the LLM\"."
  private var isError: Boolean = false
  private var asking: Boolean = false

  private def setRequest(s: String): Unit = { request = s; rerender() }

  private def dispatch(cmd: Command): Unit = { sketch = EtchASketchModel(sketch, cmd); rerender() }

  private def resetSketch(): Unit = { sketch = EtchASketchModel.empty; rerender() }

  private def promptFor(subject: String): String =
    s"""You are controlling a simple pen-plotter, like an Etch-A-Sketch. It has a 280x280 drawing
       |area, centred on (0,0): x increases to the right, y increases downward, both axes run
       |roughly -140..140.
       |
       |The only tools you have are (this is the same shape a real MCP server's tools/list reply
       |would take):
       |
       |${EtchASketchModel.toolListJson}
       |
       |Reply with ONLY a JSON array of tool calls, no prose and no markdown fences, e.g.
       |[{"tool": "pen_down", "args": {}}, {"tool": "move_right", "args": {"steps": 40}}]
       |
       |Use at most 60 tool calls. Keep the whole drawing inside -130..130 on both axes. It's fine
       |to lift and lower the pen to draw several separate strokes.
       |
       |Draw: ${subject.trim}""".stripMargin

  private def ask(subject: String): Unit =
    val p = promptFor(subject)
    sentPrompt = Some(p)
    rawResponse = None
    status = "Waiting for the LLM... (puter.js may open a small sign-in popup the first time - it's free, no card, just a username)"
    isError = false
    asking = true
    rerender()

    Puter.chat(p).onComplete {
      case Success(text) =>
        rawResponse = Some(text)
        EtchASketchModel.parseCommands(text) match
          case Right(cmds) =>
            sketch = EtchASketchModel.runAll(EtchASketchModel.empty, cmds)
            status = s"Parsed and ran ${cmds.size} tool call(s)."
            isError = false
          case Left(err) =>
            status = s"Got a reply, but couldn't parse it as tool calls: $err"
            isError = true
        asking = false
        rerender()
      case Failure(e) =>
        rawResponse = None
        status = s"Request failed: ${e.getMessage}"
        isError = true
        asking = false
        rerender()
    }

  private def askPreset(s: String): Unit = { request = s; ask(s) }

  override protected def render =
    <.div(^.cls := styling.className,
      <.div(
        EtchASketchWidget.render(sketch, dispatch, resetSketch)
      ),
      <.div(
        <.div(^.cls := "md-ask-row",
          textInput(request, setRequest),
          <.button(^.cls := "btn btn-primary btn-sm", ^.prop.disabled := asking,
            ^.onClick --> ask(request), if asking then "Asking..." else "Ask the LLM to draw it")
        ),
        <.div(^.cls := "md-presets",
          <.button(^.cls := "btn btn-outline-secondary btn-sm", ^.onClick --> askPreset("a boat"), "a boat"),
          <.button(^.cls := "btn btn-outline-secondary btn-sm", ^.onClick --> askPreset("a house"), "a house"),
          <.button(^.cls := "btn btn-outline-secondary btn-sm", ^.onClick --> askPreset("a flower"), "a flower")
        ),
        LlmTrafficWidget.render(sentPrompt, rawResponse, status, isError)
      )
    )
}
