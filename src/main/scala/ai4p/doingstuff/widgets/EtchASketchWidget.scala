package ai4p.doingstuff.widgets

import com.wbillingsley.veautiful.html.{<, SVG, Styling, DSvgContent, VHtmlContent, ^}
import org.scalajs.dom
import ai4p.{*, given}

/**
 * A pure renderer for [[EtchASketchModel]] - like [[ai4p.neuralnets.widgets.NetworkView]], it's just a
 * function of the data you hand it, so it can be driven either by a person clicking the buttons or by
 * a parent widget replaying commands an LLM chose to call.
 */
object EtchASketchWidget {
  import EtchASketchModel.Command

  val styling = Styling(
    """|display: inline-block;
       |font-family: 'Lato', sans-serif;
       |""".stripMargin
  ).modifiedBy(
    " .es-canvas" -> "background: #fffdf5; border: 2px solid #333; border-radius: 4px;",
    " .es-controls" -> "display: grid; grid-template-columns: repeat(3, 44px); grid-template-rows: repeat(3, 34px); gap: 4px; margin-top: 10px; align-items: center; justify-items: center;",
    " .es-controls button" -> "width: 40px; height: 30px; padding: 0;",
    " .es-penrow" -> "display: flex; gap: 8px; margin-top: 10px;",
    " .es-hint" -> "color: #666; font-size: 0.8rem; margin-top: 6px; max-width: 320px;"
  ).register()

  private val half = 140.0 // the drawing area runs -140..140 on both axes, centred on (0,0)
  private val size = half * 2 + 20

  private def toSvg(p: (Double, Double)): String =
    val (x, y) = p
    s"${x + half + 10},${y + half + 10}"

  private def canvas(model: EtchASketchModel): DSvgContent =
    SVG.svg(^.cls := "es-canvas", ^.attr("width") := size, ^.attr("height") := size,
      SVG.g(
        model.strokes.map(stroke =>
          SVG.polyline(
            ^.attr("points") := stroke.map(toSvg).mkString(" "),
            ^.attr("fill") := "none", ^.attr("stroke") := "#1f2937",
            ^.attr("stroke-width") := "3", ^.attr("stroke-linecap") := "round", ^.attr("stroke-linejoin") := "round"
          )
        )
      ),
      SVG.circle(
        ^.attr("cx") := model.x + half + 10, ^.attr("cy") := model.y + half + 10, ^.attr("r") := 5,
        ^.attr("fill") := (if model.isPenDown then "#dc2626" else "#9ca3af"),
        ^.attr("stroke") := "#fff", ^.attr("stroke-width") := "1.5"
      )
    )

  /** Renders the sketchpad plus its manual controls. `dispatch` runs one command; `onReset` clears the pad. */
  def render(model: EtchASketchModel, dispatch: Command => Unit, onReset: () => Unit, stepSize: Double = 20): VHtmlContent =
    <.div(^.cls := styling.className,
      canvas(model),
      <.div(^.cls := "es-penrow",
        <.button(^.cls := (if model.isPenDown then "btn btn-warning btn-sm" else "btn btn-outline-success btn-sm"),
          ^.onClick --> dispatch(if model.isPenDown then Command.PenUp else Command.PenDown),
          if model.isPenDown then "Pen down (drawing)" else "Pen up (not drawing)"
        ),
        <.button(^.cls := "btn btn-outline-secondary btn-sm", ^.onClick --> onReset(), "Clear")
      ),
      <.div(^.cls := "es-controls",
        <.div(), <.button(^.cls := "btn btn-outline-primary btn-sm", ^.onClick --> dispatch(Command.MoveUp(stepSize)), "↑"), <.div(),
        <.button(^.cls := "btn btn-outline-primary btn-sm", ^.onClick --> dispatch(Command.MoveLeft(stepSize)), "←"), <.div(),
        <.button(^.cls := "btn btn-outline-primary btn-sm", ^.onClick --> dispatch(Command.MoveRight(stepSize)), "→"),
        <.div(), <.button(^.cls := "btn btn-outline-primary btn-sm", ^.onClick --> dispatch(Command.MoveDown(stepSize)), "↓"), <.div()
      ),
      <.div(^.cls := "es-hint", "This is the whole API: pen_up, pen_down, move_left/right/up/down(steps). Try it yourself, or let the LLM below drive it.")
    )
}
