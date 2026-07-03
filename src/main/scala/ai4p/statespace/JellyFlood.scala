package willtap.imperativeTopic

import com.wbillingsley.veautiful.html.{<, DHtmlComponent, ^, Styling}
import org.scalajs.dom.{Element, Node}

import scala.collection.mutable

import ai4p.{given, *}
import ai4p.statespace.widgets.{Algorithm, GCell, Pos, SearchState, step}

object JellyFlood {
  val w = 40

  styleSuite.addGlobalRules(
    """|@keyframes pulse-jelly {
       |    0% {
       |        opacity: 1;
       |        border-radius: 5px;
       |    }
       |
       |    70% {
       |        opacity: 0.6;
       |        border-radius: 20px;
       |    }
       |
       |    100% {
       |        opacity: 1;
       |        border-radius: 5px;
       |    }
       |}
       |""".stripMargin
  )
  styleSuite.update()

  val jellyGrid = Styling(
    """|
       |""".stripMargin
  ).modifiedBy(
    " .jelly-row" -> s"display: grid; grid-template-columns: repeat(auto-fit, ${w}px); font-size: ${w/2}px;",
    " .jelly-cell" -> s"width: ${w}px; height: ${w}px; border: 1px solid lightgray;",
    " .jelly-cell .floor" -> "background-color: #242424; width: 100%; height: 100%;",
    " .jelly-cell .lava" -> "background-color: darkred; width: 100%; height: 100%;",
    " .jelly-cell .jelly" -> s"background-color: darkgreen; width: 100%; height: 100%; border-radius: ${w/6}px; color: white; text-align: center; line-height: ${w}px;",
    " .jelly-cell .jelly.active" -> "animation: pulse-jelly 2s infinite;",
    " .jelly-cell .start" -> s"background-color: #3b82f6; width: 100%; height: 100%; border-radius: ${w/6}px; color: white; text-align: center; line-height: ${w}px;",
    " .jelly-cell .goal" -> s"background-color: #22c55e; width: 100%; height: 100%; border-radius: ${w/6}px; color: white; text-align: center; line-height: ${w}px;",
    " .jelly-cell .bramble" -> s"background-color: #7c4a1e; width: 100%; height: 100%;",
    " .jelly-algo-bar" -> "display: flex; gap: 6px; flex-wrap: wrap; margin-bottom: 8px;"
  ).register()

  val algoLabel: Algorithm => String = {
    case Algorithm.FloodFill => "Flood Fill"
    case Algorithm.Dijkstra  => "Dijkstra"
    case Algorithm.BFS       => "BFS"
    case Algorithm.DFS       => "DFS"
    case Algorithm.GreedyBFS => "Greedy BFS"
    case Algorithm.AStar     => "A*"
  }
}

case class JellyFlood(
  w: Int = 8, h: Int = 8,
  goalX: Int = 7, goalY: Int = 7,
  startX: Int = 0, startY: Int = 0,
  mazeString: String = """|..######
                          |........
                          |#.####.#
                          |#.####.#
                          |........
                          |#.####.#
                          |######..
                          |######..""".stripMargin,
  initialAlgo: Algorithm = Algorithm.FloodFill
) extends DHtmlComponent {

  private val maze    = mutable.Map.empty[(Int, Int), Boolean]
  private val bramble = mutable.Set.empty[(Int, Int)]

  // step number at which each cell was first discovered; drives tick animation
  private val visitOrder = mutable.Map.empty[(Int, Int), Int]

  var tick = 0
  var currentAlgo: Algorithm = initialAlgo

  private def setSquare(x: Int, y: Int, c: Char): Unit = c match {
    case '.' => maze((x, y)) = true
    case '#' => maze((x, y)) = false
    case '*' => maze((x, y)) = true; bramble += ((x, y))
    case _   => // do nothing
  }

  private def loadFromString(s: String): Unit =
    for {
      (line, y) <- s.linesIterator.zipWithIndex if y < h
      (char, x) <- line.zipWithIndex if x < w
    } setSquare(x, y, char)

  private def buildGrid(): IndexedSeq[IndexedSeq[GCell]] =
    IndexedSeq.tabulate(h, w) { (r, c) =>
      if !maze.getOrElse((c, r), false) then GCell.Wall
      else if bramble.contains((c, r))  then GCell.Bramble
      else GCell.Open
    }

  def reset(): Unit = {
    tick = 0
    maze.clear()
    bramble.clear()
    visitOrder.clear()
    loadFromString(mazeString)

    val grid     = buildGrid()
    val goalPos  = Pos(goalY, goalX)
    val startPos = Pos(startY, startX)

    // FloodFill expands from the goal outward; other algorithms search from start.
    val origin = currentAlgo match {
      case Algorithm.FloodFill => goalPos
      case _                   => startPos
    }
    // Sentinel goal for FloodFill so it runs until fringe is empty rather than stopping early.
    val searchGoal = currentAlgo match {
      case Algorithm.FloodFill => Pos(-1, -1)
      case _                   => goalPos
    }

    // Distance-based algorithms animate by actual distance value (wave bands).
    // Exploration-order algorithms animate by expansion step (one cell per tick).
    val useDist = currentAlgo match {
      case Algorithm.FloodFill | Algorithm.BFS | Algorithm.Dijkstra => true
      case _ => false
    }

    var state = SearchState.initial(origin)
    var expansionStep = 0

    if useDist then
      visitOrder((origin.col, origin.row)) = 0   // origin is at distance 0

    while !state.done && state.fringe.nonEmpty do
      val prevFringe = state.fringe.toSet
      val prevDist   = state.dist
      state = step(state, currentAlgo, grid, searchGoal)
      expansionStep += 1

      if useDist then
        for (pos, d) <- state.dist if !prevDist.contains(pos) do
          visitOrder((pos.col, pos.row)) = d
      else
        // The node that was popped = prevFringe − newFringe (new neighbours are not in prevFringe).
        val expanded = prevFringe -- state.fringe.toSet
        for pos <- expanded do
          val key = (pos.col, pos.row)
          if !visitOrder.contains(key) then visitOrder(key) = expansionStep
  }

  reset()

  override protected def render =
    <.div(^.cls := JellyFlood.jellyGrid,
      <.div(^.cls := "jelly-algo-bar",
        Algorithm.values.map { a =>
          <.button(
            ^.cls := (if a == currentAlgo then "btn btn-primary btn-sm" else "btn btn-outline-secondary btn-sm"),
            ^.onClick --> { currentAlgo = a; reset(); rerender() },
            JellyFlood.algoLabel(a)
          )
        }
      ),
      <.div(^.cls := "jelly-grid",
        for {
          y <- 0 until h
        } yield <.div(^.cls := "jelly-row",
          for {
            x <- 0 until w
          } yield {
            val d          = visitOrder.getOrElse(x -> y, Int.MaxValue)
            val isGoal     = x == goalX  && y == goalY
            val isStart    = x == startX && y == startY
            val isBramble  = bramble.contains(x -> y)
            val label      = if isGoal then "G" else if isStart then "S" else d.toString
            <.div(^.cls := "jelly-cell",
              if maze.getOrElse(x -> y, false) then
                if d < Int.MaxValue && tick > d then
                  <.div(^.cls := "jelly", label)
                else if d < Int.MaxValue && tick == d then
                  <.div(^.cls := "jelly active", label)
                else if isGoal then
                  <.div(^.cls := "goal", "G")
                else if isStart then
                  <.div(^.cls := "start", "S")
                else if isBramble then
                  <.div(^.cls := "bramble")
                else
                  <.div(^.cls := "floor")
              else <.div(^.cls := "lava")
            )
          }
        )
      ),
      <.div(^.cls := "btn-group mt-2",
        <.button(^.cls := "btn btn-outline-secondary", ^.onClick --> { tick = 0; rerender() }, "Reset"),
        <.button(^.cls := "btn btn-outline-primary",  ^.onClick --> { tick += 1; rerender() }, "Step")
      )
    )
}
