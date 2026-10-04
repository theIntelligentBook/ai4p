package ai4p.doingstuff.widgets

import com.wbillingsley.veautiful.html.{<, SVG, Styling, DSvgContent, VHtmlContent, ^}
import ai4p.{*, given}

/**
 * A static box diagram of MCP's client-server shape: one host application, talking through an MCP
 * client to any number of independent MCP servers, each advertising its own tools/resources/prompts.
 * Purely illustrative layout, like [[ai4p.embeddings.widgets.TransformerBlockDiagram]].
 */
object McpArchitectureDiagram {

  val styling = Styling(
    """|display: inline-block;
       |font-family: 'Lato', sans-serif;
       |""".stripMargin
  ).register()

  private case class Server(name: String, capabilities: String, fill: String)

  private val servers = Vector(
    Server("Filesystem server", "tools: read_file, write_file, list_dir", "#dbeafe"),
    Server("GitHub server", "tools: create_issue, search_code, ...", "#dcfce7"),
    Server("Drawing server (this demo)", "tools: pen_up, pen_down, move_*", "#fef3c7")
  )

  private val w = 780
  private val hostW = 300; private val hostH = 64; private val hostX = (w - hostW) / 2.0; private val hostY = 20
  private val boxW = 230; private val boxH = 64; private val boxY = 220
  private val gap = (w - servers.size * boxW) / (servers.size + 1).toDouble

  private def boxX(i: Int): Double = gap * (i + 1) + boxW * i

  private def doubleArrow(x: Double, y1: Double, y2: Double): DSvgContent =
    SVG.g(
      SVG.line(^.attr("x1") := x, ^.attr("y1") := y1, ^.attr("x2") := x, ^.attr("y2") := y2,
        ^.attr("stroke") := "#999", ^.attr("stroke-width") := "2"),
      SVG.polygon(^.attr("points") := s"$x,${y2 + 7} ${x - 6},${y2 - 4} ${x + 6},${y2 - 4}", ^.attr("fill") := "#999"),
      SVG.polygon(^.attr("points") := s"$x,${y1 - 7} ${x - 6},${y1 + 4} ${x + 6},${y1 + 4}", ^.attr("fill") := "#999"),
      SVG.text(^.attr("x") := x + 10, ^.attr("y") := (y1 + y2) / 2, ^.attr("font-size") := "13",
        ^.attr("font-family") := "monospace", ^.attr("fill") := "#777", "JSON-RPC")
    )

  private def box(x: Double, y: Double, width: Double, height: Double, fill: String, title: String, sub: String): DSvgContent =
    SVG.g(
      SVG.rect(^.attr("x") := x, ^.attr("y") := y, ^.attr("width") := width, ^.attr("height") := height,
        ^.attr("rx") := 9, ^.attr("fill") := fill, ^.attr("stroke") := "#888", ^.attr("stroke-width") := "1"),
      SVG.text(^.attr("x") := x + width / 2, ^.attr("y") := y + 27, ^.attr("text-anchor") := "middle",
        ^.attr("font-size") := "17", ^.attr("fill") := "#222", title),
      SVG.text(^.attr("x") := x + width / 2, ^.attr("y") := y + 47, ^.attr("text-anchor") := "middle",
        ^.attr("font-size") := "11", ^.attr("font-family") := "monospace", ^.attr("fill") := "#666", sub)
    )

  def svg: DSvgContent =
    val h = boxY + boxH + 20
    SVG.svg(^.attr("width") := w, ^.attr("height") := h,
      SVG.g(servers.zipWithIndex.map((_, i) => doubleArrow(boxX(i) + boxW / 2, hostY + hostH, boxY))*),
      box(hostX, hostY, hostW, hostH, "#ede9fe", "Host app + MCP client", "e.g. an LLM-powered editor or agent"),
      SVG.g(servers.zipWithIndex.map((s, i) => box(boxX(i), boxY, boxW, boxH, s.fill, s.name, s.capabilities))*)
    )

  def render: VHtmlContent = <.div(^.cls := styling.className, svg)
}
