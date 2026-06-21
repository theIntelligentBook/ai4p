package ai4p.statespace.widgets

import com.wbillingsley.veautiful.html.{<, SVG, ^, DHtmlComponent}
import com.wbillingsley.veautiful.html.VHtmlContent
import com.wbillingsley.veautiful.html.DSvgContent

// --- Game logic ---


// --- Widget ---

/** Noughts and Crosses widget.
  * @param xGoesFirst if true, Cross plays first; if false, Nought plays first.
  */
case class NoughtsAndCrosses(xGoesFirst: Boolean) extends DHtmlComponent:

  import TicTacToe.*
  import Player.*
  import Cell.*

  private val firstPlayer = if xGoesFirst then Cross else Nought

  private var board: Board = empty
  private var current: Player = firstPlayer

  private def reset(): Unit =
    board = empty
    current = firstPlayer
    rerender()

  private def handleClick(idx: Int): Unit =
    if winner(board).isEmpty && !isDraw(board) then
      board = play(board, idx, current)
      if winner(board).isEmpty && !isDraw(board) then
        current = if current == Nought then Cross else Nought
      rerender()

  private def cellSymbol(c: Cell, idx: Int): DSvgContent =
    val size = 80
    val cx = 40; val cy = 40
    c match
      case Empty =>
        SVG.rect(
          ^.attr("x") := 2, ^.attr("y") := 2,
          ^.attr("width") := size - 4, ^.attr("height") := size - 4,
          ^.attr("fill") := "#f8f8f8", ^.attr("stroke") := "#ccc",
          ^.attr("rx") := 6,
          ^.on("click") ==> (_ => handleClick(idx)),
          ^.attr("style") := "cursor:pointer;"
        )
      case O =>
        SVG.g(
          SVG.rect(
            ^.attr("x") := 2, ^.attr("y") := 2,
            ^.attr("width") := size - 4, ^.attr("height") := size - 4,
            ^.attr("fill") := "#eef6ff", ^.attr("stroke") := "#ccc",
            ^.attr("rx") := 6
          ),
          SVG.circle(
            ^.attr("cx") := cx, ^.attr("cy") := cy, ^.attr("r") := 26,
            ^.attr("stroke") := "#3b82f6", ^.attr("stroke-width") := 5,
            ^.attr("fill") := "none"
          )
        )
      case X =>
        SVG.g(
          SVG.rect(
            ^.attr("x") := 2, ^.attr("y") := 2,
            ^.attr("width") := size - 4, ^.attr("height") := size - 4,
            ^.attr("fill") := "#fff5f5", ^.attr("stroke") := "#ccc",
            ^.attr("rx") := 6
          ),
          SVG.line(
            ^.attr("x1") := 18, ^.attr("y1") := 18,
            ^.attr("x2") := 62, ^.attr("y2") := 62,
            ^.attr("stroke") := "#ef4444", ^.attr("stroke-width") := 5,
            ^.attr("stroke-linecap") := "round"
          ),
          SVG.line(
            ^.attr("x1") := 62, ^.attr("y1") := 18,
            ^.attr("x2") := 18, ^.attr("y2") := 62,
            ^.attr("stroke") := "#ef4444", ^.attr("stroke-width") := 5,
            ^.attr("stroke-linecap") := "round"
          )
        )

  def render =
    val w = winner(board)
    val draw = isDraw(board)

    val statusMsg = w match
      case Some(Nought) => "⭕ Nought wins!"
      case Some(Cross)  => "❌ Cross wins!"
      case None if draw => "It's a draw!"
      case None         => s"${if current == Nought then "⭕ Nought" else "❌ Cross"}'s turn"

    <.div(^.attr("style") := "display:inline-block; text-align:center; font-family:sans-serif;",
      <.p(^.attr("style") := "font-size:1.1em; margin-bottom:8px;", statusMsg),
      <.svg(
        ^.attr("width") := 252, ^.attr("height") := 252,
        ^.attr("viewBox") := "0 0 252 252",
        (for
          row <- 0 until 3
          col <- 0 until 3
          idx = row * 3 + col
        yield
          SVG.g(
            ^.attr("transform") := s"translate(${col * 84}, ${row * 84})",
            cellSymbol(board(idx), idx)
          )
        )
      ),
      <.div(^.attr("style") := "margin-top:10px;",
        <.button(
          ^.attr("style") := "padding:6px 18px; font-size:0.95em; cursor:pointer;",
          ^.on("click") ==> (_ => reset()),
          "Reset"
        )
      )
    )