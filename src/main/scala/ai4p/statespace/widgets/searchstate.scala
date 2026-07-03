package ai4p.statespace.widgets

import com.wbillingsley.veautiful.html.{ <, SVG, ^, DHtmlComponent, Styling }
import org.scalajs.dom

import ai4p.{*, given}

// ---- Maze model ----

enum GCell:
  case Open, Wall, Start, Goal, Bramble

def cellCost(cell: GCell): Int = cell match
  case GCell.Bramble => 3
  case _             => 1

case class Pos(row: Int, col: Int)

/** Parse a maze from a multiline string.
 *  '#' = wall, 'S' = start, 'G' = goal, anything else = open.
 */
def parseMaze(s: String): (IndexedSeq[IndexedSeq[GCell]], Pos, Pos) =
  val rows = s.stripMargin.linesIterator.filter(_.nonEmpty).toIndexedSeq
  var start = Pos(0, 0)
  var goal  = Pos(0, 0)
  val grid  = rows.zipWithIndex.map { (row, r) =>
    row.zipWithIndex.map { (ch, c) =>
      ch match
        case '#' => GCell.Wall
        case 'S' => start = Pos(r, c); GCell.Start
        case 'G' => goal  = Pos(r, c); GCell.Goal
        case '*' => GCell.Bramble
        case _   => GCell.Open
    }.toIndexedSeq
  }
  (grid, start, goal)

// ---- Search algorithms ----

enum Algorithm:
  case BFS, DFS, GreedyBFS, FloodFill, AStar, Dijkstra

/** One step of search state. */
case class SearchState(
  dist:    Map[Pos, Int],          // settled distances
  fringe:  Seq[Pos],               // current frontier (ordered)
  parent:  Map[Pos, Pos],          // for path reconstruction
  done:    Boolean
)

object SearchState:
  def initial(start: Pos): SearchState =
    SearchState(Map(start -> 0), Seq(start), Map.empty, false)

def neighbours(p: Pos, grid: IndexedSeq[IndexedSeq[GCell]]): Seq[Pos] =
  val rows = grid.size; val cols = grid(0).size
  Seq(Pos(p.row-1,p.col), Pos(p.row+1,p.col), Pos(p.row,p.col-1), Pos(p.row,p.col+1))
    .filter(n => n.row >= 0 && n.row < rows && n.col >= 0 && n.col < cols
              && grid(n.row)(n.col) != GCell.Wall)

def heuristic(p: Pos, goal: Pos): Int =
  Math.abs(p.row - goal.row) + Math.abs(p.col - goal.col)

def step(
  state: SearchState,
  algo:  Algorithm,
  grid:  IndexedSeq[IndexedSeq[GCell]],
  goal:  Pos
): SearchState =
  if state.done || state.fringe.isEmpty then return state.copy(done = true)

  // Pick the node to expand based on algorithm
  val current = algo match
    case Algorithm.DFS        => state.fringe.last
    case Algorithm.GreedyBFS  => state.fringe.minBy(p => heuristic(p, goal))
    case Algorithm.AStar      => state.fringe.minBy(p => state.dist(p) + heuristic(p, goal))
    case Algorithm.Dijkstra   => state.fringe.minBy(p => state.dist(p))
    case _                    => state.fringe.head   // BFS / FloodFill

  val remaining = algo match
    case Algorithm.DFS        => state.fringe.init
    case Algorithm.GreedyBFS  => state.fringe.filterNot(_ == current)
    case Algorithm.AStar      => state.fringe.filterNot(_ == current)
    case Algorithm.Dijkstra   => state.fringe.filterNot(_ == current)
    case _                    => state.fringe.tail

  val currentDist = state.dist(current)
  val newNeighbours = neighbours(current, grid).filterNot(state.dist.contains)

  // Dijkstra uses actual cell entry costs; all other algorithms treat all moves as 1.
  def moveCost(n: Pos): Int = algo match
    case Algorithm.Dijkstra => cellCost(grid(n.row)(n.col))
    case Algorithm.AStar => cellCost(grid(n.row)(n.col))
    case _                  => 1

  val newDist   = state.dist   ++ newNeighbours.map(n => n -> (currentDist + moveCost(n)))
  val newParent = state.parent ++ newNeighbours.map(_ -> current)
  val newFringe = remaining    ++ newNeighbours

  val reachedGoal = newDist.contains(goal)
  SearchState(newDist, newFringe, newParent, reachedGoal || newFringe.isEmpty)

def reconstructPath(goal: Pos, parent: Map[Pos, Pos], start: Pos): Set[Pos] =
  def loop(p: Pos, acc: Set[Pos]): Set[Pos] =
    if p == start then acc + p
    else parent.get(p) match
      case Some(prev) => loop(prev, acc + p)
      case None       => acc
  loop(goal, Set.empty)

// ---- Styling ----

val mazeStyle = Styling(
  """|display: inline-block;
     |font-family: 'Lato', sans-serif;
     |""".stripMargin
).modifiedBy(
  " .maze-controls" -> "display: flex; gap: 0.6em; align-items: center; margin-bottom: 0.5em; flex-wrap: wrap;",
  " select"         -> "padding: 3px 6px; font-size: 0.9em;",
  " button"         -> "padding: 4px 14px; font-size: 0.9em; cursor: pointer; border-radius: 4px; border: 1px solid #aaa; background: #f5f5f5;",
  " button:hover"   -> "background: #e0e0e0;",
  " .maze-label"    -> "font-size: 0.85em; color: #555;"
).register()

// ---- Widget ----

/** Interactive maze search widget.
 *
 *  @param mazeStr  Multiline string; '#'=wall, 'S'=start, 'G'=goal, ' '=open.
 *  @param cellSize Pixel size of each cell (default 36).
 *  @param algo     Initial algorithm (default BFS).
 *
 *  Example maze string:
 *  {{{
 *    |#########
 *    |#S  #   #
 *    |# ## # ##
 *    |#    #  #
 *    |#### #  #
 *    |#       #
 *    |# ## ## #
 *    |#   #  G#
 *    |#########
 *  }}}
 */
case class MazeSearch(
  mazeStr:  String,
  cellSize: Int       = 36,
  algo:     Algorithm = Algorithm.BFS
) extends DHtmlComponent:

  private val (grid, start, goal) = parseMaze(mazeStr)
  private val rows = grid.size
  private val cols = grid(0).size

  private var currentAlgo  = algo
  private var searchState  = SearchState.initial(start)
  private var pulsePhase   = 0   // 0..3, toggled by a timer for the fringe pulse
  private var timerId: Option[Int] = None

  // Start a CSS-class pulse timer
  private def startPulse(): Unit =
    if timerId.isEmpty then
      timerId = Some(dom.window.setInterval(() => {
        pulsePhase = (pulsePhase + 1) % 4
        rerender()
      }, 400).toInt)

  private def stopPulse(): Unit =
    timerId.foreach(id => dom.window.clearInterval(id))
    timerId = None

  override def afterAttach(): Unit = startPulse()
  override def beforeDetach(): Unit = stopPulse()

  private def reset(): Unit =
    searchState = SearchState.initial(start)
    rerender()

  private def stepOnce(): Unit =
    if !searchState.done then
      searchState = step(searchState, currentAlgo, grid, goal)
      rerender()

  private def cellFill(p: Pos, fringeSet: Set[Pos], pathSet: Set[Pos]): String =
    val cell      = grid(p.row)(p.col)
    val isBramble = cell == GCell.Bramble
    if pathSet.contains(p)     then "#f59e0b"
    else if p == goal          then "#22c55e"
    else if p == start         then "#3b82f6"
    else if cell == GCell.Wall then "#374151"
    else if fringeSet.contains(p) then
      if isBramble then (if pulsePhase % 2 == 0 then "#92400e" else "#b45309")
      else              (if pulsePhase % 2 == 0 then "#fb923c" else "#fdba74")
    else if searchState.dist.contains(p) then
      if isBramble then "#c9a87c" else "#bfdbfe"
    else
      if isBramble then "#7c4a1e" else "#f9fafb"

  private def distLabel(p: Pos): Option[String] =
    if grid(p.row)(p.col) == GCell.Wall then None
    else searchState.dist.get(p).map(_.toString)

  def render =
    val fringeSet = searchState.fringe.toSet
    val pathSet   =
      if searchState.dist.contains(goal) then
        reconstructPath(goal, searchState.parent, start)
      else Set.empty[Pos]

    val svgW = cols * cellSize
    val svgH = rows * cellSize
    val cs   = cellSize
    val fs   = Math.max(8, cs / 3)   // font size for distance numbers

    <.div(^.cls := mazeStyle,
      <.div(^.cls := "maze-controls",
        <.span(^.cls := "maze-label", "Algorithm:"),
        <.select(
          ^.on("change") ==> { e =>
            val v = e.target.asInstanceOf[dom.html.Select].value
            currentAlgo = Algorithm.valueOf(v)
            reset()
          },
          Algorithm.values.map { a =>
            val label = a match
              case Algorithm.BFS       => "Breadth-First Search"
              case Algorithm.DFS       => "Depth-First Search"
              case Algorithm.GreedyBFS => "Greedy Best-First"
              case Algorithm.FloodFill => "Flood Fill"
              case Algorithm.AStar     => "A*"
              case Algorithm.Dijkstra  => "Dijkstra's"
            <.option(
              ^.attr("value") := a.toString,
              ^.attr("selected") ?= (if a == currentAlgo then Some("") else None),
              label
            )
          }
        ),
        <.button(^.on("click") ==> (_ => stepOnce()), "Step"),
        <.button(^.on("click") ==> (_ => reset()),     "Reset"),
        if searchState.done then
          <.span(^.cls := "maze-label",
            if searchState.dist.contains(goal) then
              s" Done — path length ${searchState.dist(goal)}"
            else " Done — no path found"
          )
        else
          <.span(^.cls := "maze-label",
            s" Fringe: ${searchState.fringe.size}"
          )
      ),
      <.svg(
        ^.attr("width")   := svgW,
        ^.attr("height")  := svgH,
        ^.attr("viewBox") := s"0 0 $svgW $svgH",
        // Cells
        for
          r <- 0 until rows
          c <- 0 until cols
          p  = Pos(r, c)
        yield SVG.g(
          SVG.rect(
            ^.attr("x")      := c * cs,
            ^.attr("y")      := r * cs,
            ^.attr("width")  := cs,
            ^.attr("height") := cs,
            ^.attr("fill")   := cellFill(p, fringeSet, pathSet),
            ^.attr("stroke") := "#d1d5db",
            ^.attr("stroke-width") := "1"
          ),
          // Distance number
          distLabel(p).map { lbl =>
            SVG.text(
              ^.attr("x")           := c * cs + cs / 2,
              ^.attr("y")           := r * cs + cs / 2 + fs / 3,
              ^.attr("text-anchor") := "middle",
              ^.attr("font-size")   := fs,
              ^.attr("fill")        := (if pathSet.contains(p) then "#7c2d12" else "#1e3a5f"),
              ^.attr("font-family") := "monospace",
              lbl
            )
          }.getOrElse(SVG.g()),
          // S / G labels
          if p == start then
            SVG.text(
              ^.attr("x") := c * cs + cs / 2, ^.attr("y") := r * cs + cs / 2 + fs / 3,
              ^.attr("text-anchor") := "middle", ^.attr("font-size") := fs,
              ^.attr("fill") := "white", ^.attr("font-weight") := "bold",
              ^.attr("font-family") := "sans-serif", "S"
            )
          else if p == goal then
            SVG.text(
              ^.attr("x") := c * cs + cs / 2, ^.attr("y") := r * cs + cs / 2 + fs / 3,
              ^.attr("text-anchor") := "middle", ^.attr("font-size") := fs,
              ^.attr("fill") := "white", ^.attr("font-weight") := "bold",
              ^.attr("font-family") := "sans-serif", "G"
            )
          else SVG.g()
        ),
        // Path overlay — draw connecting lines through cell centres
        if pathSet.size > 1 then
          val pathSeq = {
            def chain(p: Pos, acc: List[Pos]): List[Pos] =
              searchState.parent.get(p) match
                case Some(prev) => chain(prev, p :: acc)
                case None       => p :: acc
            chain(goal, Nil)
          }
          val pts = pathSeq.map(p => s"${p.col * cs + cs/2},${p.row * cs + cs/2}").mkString(" ")
          SVG.polyline(
            ^.attr("points")       := pts,
            ^.attr("fill")         := "none",
            ^.attr("stroke")       := "#b45309",
            ^.attr("stroke-width") := "3",
            ^.attr("stroke-linecap")  := "round",
            ^.attr("stroke-linejoin") := "round",
            ^.attr("opacity")      := "0.75"
          )
        else SVG.g()
      )
    )