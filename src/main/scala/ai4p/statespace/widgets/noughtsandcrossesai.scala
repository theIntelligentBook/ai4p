package ai4p.statespace.widgets

import com.wbillingsley.veautiful.html.{<, SVG, ^, DHtmlComponent}
import com.wbillingsley.veautiful.html.VHtmlContent
import com.wbillingsley.veautiful.html.DSvgContent

// --- Game logic ---

enum Player:
  case Nought, Cross

enum Cell:
  case Empty, O, X

type Board = Vector[Cell]

object TicTacToe:

  val empty: Board = Vector.fill(9)(Cell.Empty)

  val lines = Seq(
    Seq(0,1,2), Seq(3,4,5), Seq(6,7,8),
    Seq(0,3,6), Seq(1,4,7), Seq(2,5,8),
    Seq(0,4,8), Seq(2,4,6)
  )

  def winner(b: Board): Option[Player] =
    lines.collectFirst {
      case line if line.map(b).forall(_ == Cell.O) => Player.Nought
      case line if line.map(b).forall(_ == Cell.X) => Player.Cross
    }

  def isDraw(b: Board): Boolean =
    b.forall(_ != Cell.Empty) && winner(b).isEmpty

  def isTerminal(b: Board): Boolean =
    winner(b).isDefined || isDraw(b)

  def cellOf(p: Player): Cell =
    if p == Player.Nought then Cell.O else Cell.X

  def play(b: Board, idx: Int, p: Player): Board =
    if b(idx) == Cell.Empty then b.updated(idx, cellOf(p)) else b

  def legalMoves(b: Board): Seq[Int] =
    b.indices.filter(b(_) == Cell.Empty)

  // Minimax: returns (score, bestIndex)
  // score is from the perspective of `maximiser`
  def minimax(b: Board, current: Player, maximiser: Player, depth: Int): (Int, Int) =
    winner(b) match
      case Some(p) =>
        val score = if p == maximiser then 10 - depth else depth - 10
        (score, -1)
      case None if isDraw(b) => (0, -1)
      case None =>
        val moves = legalMoves(b)
        val scored = moves.map { idx =>
          val next = play(b, idx, current)
          val nextPlayer = if current == Player.Nought then Player.Cross else Player.Nought
          val (s, _) = minimax(next, nextPlayer, maximiser, depth + 1)
          (s, idx)
        }
        if current == maximiser then scored.maxBy(_._1)
        else scored.minBy(_._1)

  def bestMove(b: Board, aiPlayer: Player): Int =
    val nextHuman = if aiPlayer == Player.Nought then Player.Cross else Player.Nought
    // whose turn is it? count cells placed
    val placed = b.count(_ != Cell.Empty)
    // Cross always goes first, so if placed is even it's Cross's turn
    val current = if placed % 2 == 0 then Player.Cross else Player.Nought
    val (_, idx) = minimax(b, current, aiPlayer, 0)
    idx

// --- Widget ---

/** Noughts and Crosses with minimax AI.
  * Cross always goes first.
  * @param aiPlaysAs if Player.Cross the AI plays X (and moves first); if Player.Nought the AI plays O.
  */
case class NoughtsAndCrossesAI(aiPlaysAs: Player) extends DHtmlComponent:

  import TicTacToe.*
  import Player.*
  import Cell.*

  private var board: Board = empty
  // Cross always goes first
  private var current: Player = Cross

  override def afterAttach(): Unit = maybeAiMove()

  private def humanPlayer: Player = if aiPlaysAs == Cross then Nought else Cross

  private def maybeAiMove(): Unit =
    if current == aiPlaysAs && winner(board).isEmpty && !isDraw(board) then
      val idx = bestMove(board, aiPlaysAs)
      board = play(board, idx, aiPlaysAs)
      current = humanPlayer
      rerender()

  private def handleClick(idx: Int): Unit =
    if current == humanPlayer && board(idx) == Empty
        && winner(board).isEmpty && !isDraw(board) then
      board = play(board, idx, humanPlayer)
      current = aiPlaysAs
      rerender()
      maybeAiMove()

  private def reset(): Unit =
    board = empty
    current = Cross
    rerender()
    maybeAiMove()

  private def cellContent(c: Cell, idx: Int): DSvgContent =
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
      case None =>
        val whose = if current == humanPlayer then "Your" else "AI's"
        val symbol = if current == Nought then "⭕" else "❌"
        s"$whose turn ($symbol)"

    <.div(^.attr("style") := "display:inline-block; text-align:center; font-family:sans-serif;",
      <.p(^.attr("style") := "font-size:1em; color:#555; margin-bottom:4px;",
        s"You are playing ${if humanPlayer == Nought then "⭕ Noughts" else "❌ Crosses"}"
      ),
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
            cellContent(board(idx), idx)
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