package ai4p.doingstuff.widgets

import com.wbillingsley.veautiful.html.{<, Styling, VHtmlContent, ^}
import ai4p.{*, given}

/**
 * A pure renderer for one request/response round-trip with the LLM: exactly the prompt text
 * (tool list included) that was sent, and exactly what came back, before any of it gets
 * interpreted as drawing commands. This is the "wire" a real MCP client/server would carry -
 * here it's just a prompt string, because there's no real server behind the demo, but the
 * shape of the exchange (here is what you can call; here is what I decided to call) is the same.
 */
object LlmTrafficWidget {

  val styling = Styling(
    """|display: inline-block;
       |font-family: 'Lato', sans-serif;
       |max-width: 520px;
       |""".stripMargin
  ).modifiedBy(
    " .lt-col-title" -> "font-weight: bold; color: #5a074f; margin: 8px 0 4px;",
    " .lt-box" -> "width: 100%; height: 130px; font-family: monospace; font-size: 0.72rem; padding: 6px; border: 1px solid #bbb; border-radius: 4px; background: #f8f8f8; box-sizing: border-box; resize: vertical;",
    " .lt-status" -> "margin-top: 8px; font-size: 0.85rem;",
    " .lt-status.lt-error" -> "color: #991b1b;",
    " .lt-status.lt-ok" -> "color: #166534;"
  ).register()

  def render(sent: Option[String], received: Option[String], status: String, isError: Boolean): VHtmlContent =
    <.div(^.cls := styling.className,
      <.div(^.cls := "lt-col-title", "Sent to the LLM (prompt + tool list)"),
      <.textarea(^.cls := "lt-box",
        sent.getOrElse(""), ^.prop.value := sent.getOrElse(""), ^.attr("readonly") := "readonly"),
      <.div(^.cls := "lt-col-title", "Came back (raw reply)"),
      <.textarea(^.cls := "lt-box",
        received.getOrElse(""), ^.prop.value := received.getOrElse(""), ^.attr("readonly") := "readonly"),
      <.div(^.cls := s"lt-status ${if isError then "lt-error" else "lt-ok"}", status)
    )
}
