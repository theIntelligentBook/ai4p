package willtap.imperativeTopic

import com.wbillingsley.veautiful.html.{<, DHtmlComponent, ^, Styling}
import org.scalajs.dom.{Element, Node}

import scala.collection.mutable

import ai4p.{given, *}
import willtap.imperativeTopic.JellyFlood.Square

object JellyFlood {
  val w = 40

  enum Square:
    case Empty, Bramble, Wall

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
    " .jelly-cell" -> s"width: ${w}px; height: ${w}px; border: 1px solid lightgray; line-height: ${w}px; text-align: center;",
    " .jelly-cell .floor" -> "background-color: #242424; width: 100%; height: 100%;",
    " .jelly-cell .lava" -> "background-color: darkred; width: 100%; height: 100%;",
    " .jelly-cell .bramble" -> s"background-color: #7c4a1e; width: 100%; height: 100%;",
    " .jelly-cell .jelly" -> s"background-color: darkgreen; width: 100%; height: 100%; border-radius: ${w/6}px; color: white; text-align: center; line-height: ${w}px;",
    " .jelly-cell .jelly.active" -> "animation: pulse-jelly 2s infinite;",
    " .jelly-cell .visited" -> s"background-color: #3b82f6; width: 100%; height: 100%; color: white; text-align: center; line-height: ${w}px; font-size: ${w*2/3}px;",
    " .jelly-cell .visited.bramble" -> s"background-color: #7c4a1e; width: 100%; height: 100%; color: white; text-align: center; line-height: ${w}px; font-size: ${w*2/3}px;",
    " .jelly-cell .path" -> s"background-color: #f59e0b; width: 100%; height: 100%;",
    " .jelly-cell .path.bramble" -> s"background-color: #b45309; width: 100%; height: 100%;",
    " .jelly-cell .path.active" -> s"background-color: #f97316; width: 100%; height: 100%; animation: pulse-jelly 1s infinite;",
    " .jelly-cell .path.found" -> s"background-color: #4ade80; width: 100%; height: 100%;",
    " .jelly-cell .start" -> s"background-color: #8b5cf6; width: 100%; height: 100%; border-radius: ${w/6}px; color: white; text-align: center; line-height: ${w}px;",
    " .jelly-cell .goal" -> s"background-color: #22c55e; width: 100%; height: 100%; border-radius: ${w/6}px; color: white; text-align: center; line-height: ${w}px;",
    " .jelly-algo-bar" -> "display: flex; gap: 6px; flex-wrap: wrap; margin-bottom: 8px;"
  ).register()

  
}

case class JellyFlood(w:Int=8, h:Int=8, startX:Int=0, startY:Int=0, goalX:Int = 7, goalY:Int = 7, mazeString:String = "") extends DHtmlComponent {

  val maze = mutable.Map.empty[(Int, Int), JellyFlood.Square]
  val distance = mutable.Map.empty[(Int, Int), Int]
  var tick = 0

  def setSquare(x:Int, y:Int, c:Char):Unit = c match {
    case '.' => maze((x, y)) = JellyFlood.Square.Empty
    case '*' => maze((x, y)) = JellyFlood.Square.Bramble
    case '#' => maze((x, y)) = JellyFlood.Square.Wall
    case _ => // do nothing
  }

  def loadFromString(s:String) = {
    for {
      (line, y) <- s.linesIterator.zipWithIndex if y < h
      (char, x) <- line.zipWithIndex if x < w
    } {
      setSquare(x, y, char)
    }
  }


  private def check(p:(Int, Int), dist:Int):Unit = {
    distance(p) = dist
    val (x, y) = p

    for {
      (dx, dy) <- Seq((x+1, y), (x-1, y), (x, y+1), (x, y-1)) if (
        distance.getOrElse((dx, dy), Int.MaxValue) > dist + 1 &&
          maze.get(p).contains(JellyFlood.Square.Empty)
        )
    } check((dx, dy), dist + 1)
  }

  def reset(): Unit = {
    tick = 0
    maze.clear()
    distance.clear()
    loadFromString(mazeString)
    check((goalX, goalY), 0)
  }

  reset()

  override protected def render = <.div(^.cls := JellyFlood.jellyGrid,
    <.div(^.cls := "jelly-grid",
      for {
        y <- 0 until h
      } yield <.div(^.cls := "jelly-row",
        for {
          x <- 0 until w
        } yield {
          val d =  distance.getOrElse(x -> y, Int.MaxValue)

          <.div(^.cls := "jelly-cell",
            if (maze.get(x -> y).contains(JellyFlood.Square.Empty)) {
              if (tick > d) <.div(^.cls := "jelly", d.toString)
              else if (tick == d) <.div(^.cls := "jelly active", d.toString)
              else <.div(^.cls := "floor")
            } else <.div(^.cls := "lava")
          )
        }
      )
    ),
    <.div(^.cls := "btn-group",
      <.button(^.cls := "btn btn-outline-secondary", ^.onClick --> { tick = 0; rerender() }, "Reset"),
      <.button(^.cls := "btn btn-outline-primary", ^.onClick --> {
        tick = tick + 1
        rerender()
      }, "Step")
    )
  )
}

object SearchGrid {
  enum Algorithm:
    case BFS, DFS, GreedyBFS, AStar, Dijkstra

  val algoLabel: Algorithm => String = {
    case Algorithm.Dijkstra  => "Dijkstra"
    case Algorithm.BFS       => "BFS"
    case Algorithm.DFS       => "DFS"
    case Algorithm.GreedyBFS => "Greedy BFS"
    case Algorithm.AStar     => "A*"
  }
}

case class SearchGrid(w:Int=8, h:Int=8, startX:Int=0, startY:Int=0, goalX:Int=7, goalY:Int=7, mazeString:String="", algorithm:SearchGrid.Algorithm) extends DHtmlComponent {

  import SearchGrid.Algorithm
  import JellyFlood.Square

  val maze = mutable.Map.empty[(Int, Int), Square]
  // Each entry is a path (sequence of positions) from start to current node
  val frontier = mutable.ArrayDeque.empty[Seq[(Int, Int)]]
  var activePath: Option[Seq[(Int, Int)]] = None
  var foundPath: Option[Seq[(Int, Int)]] = None
  // Best cost seen so far for reaching each square
  val bestCost = mutable.Map.empty[(Int, Int), Int]

  def setSquare(x: Int, y: Int, c: Char): Unit = c match {
    case '.' => maze((x, y)) = Square.Empty
    case '*' => maze((x, y)) = Square.Bramble
    case '#' => maze((x, y)) = Square.Wall
    case _   =>
  }

  def loadFromString(s: String): Unit = {
    for {
      (line, y) <- s.linesIterator.zipWithIndex if y < h
      (char, x) <- line.zipWithIndex if x < w
    } setSquare(x, y, char)
  }

  def moveCost(sq: Square): Int = sq match {
    case Square.Bramble => 5
    case _              => 1
  }

  def pathCost(path: Seq[(Int, Int)]): Int =
    path.tail.foldLeft(0)((acc, pos) => acc + maze.get(pos).map(moveCost).getOrElse(1))

  def heuristic(pos: (Int, Int)): Int =
    math.abs(pos._1 - goalX) + math.abs(pos._2 - goalY)

  def priority(path: Seq[(Int, Int)]): Int = algorithm match {
    case Algorithm.Dijkstra  => pathCost(path)
    case Algorithm.GreedyBFS => heuristic(path.last)
    case Algorithm.AStar     => pathCost(path) + heuristic(path.last)
    case _                   => 0
  }

  private def extractNext(): Seq[(Int, Int)] = algorithm match {
    case Algorithm.DFS => frontier.removeLast()
    case Algorithm.BFS => frontier.removeHead()
    case _ =>
      val idx = frontier.indices.minBy(i => priority(frontier(i)))
      frontier.remove(idx)
  }

  private def step(): Unit = {
    if foundPath.isDefined || frontier.isEmpty then return

    val path = extractNext()
    activePath = Some(path)
    val pos = path.last
    val currentCost = pathCost(path)

    if pos == (goalX, goalY) then
      foundPath = Some(path)
      frontier.clear()
    else if currentCost <= bestCost.getOrElse(pos, Int.MaxValue) then
      val (x, y) = pos
      for (nx, ny) <- Seq((x+1,y),(x-1,y),(x,y+1),(x,y-1)) do
        if nx >= 0 && nx < w && ny >= 0 && ny < h then
          maze.get((nx, ny)) match
            case Some(Square.Wall) | None => // skip walls and out-of-bounds
            case Some(sq) =>
              val newCost = currentCost + moveCost(sq)
              if newCost < bestCost.getOrElse((nx, ny), Int.MaxValue) then
                bestCost((nx, ny)) = newCost
                frontier.append(path :+ (nx, ny))
  }

  def reset(): Unit = {
    frontier.clear()
    maze.clear()
    bestCost.clear()
    activePath = None
    foundPath = None
    loadFromString(mazeString)
    bestCost((startX, startY)) = 0
    frontier.append(Seq((startX, startY)))
  }

  reset()

  override protected def render = <.div(^.cls := JellyFlood.jellyGrid,
    <.div(^.cls := "jelly-grid",
      for y <- 0 until h yield <.div(^.cls := "jelly-row",
        for x <- 0 until w yield {
          val pos = (x, y)
          val sq = maze.get(pos)
          val isWall = sq.contains(Square.Wall) || sq.isEmpty
          val isBramble = sq.contains(Square.Bramble)
          val onActivePath = activePath.exists(_.contains(pos))
          val isActiveHead = activePath.exists(_.last == pos)
          val onFoundPath = foundPath.exists(_.contains(pos))

          <.div(^.cls := "jelly-cell",
            if pos == (goalX, goalY) then <.div(^.cls := "goal", "G")
            else if pos == (startX, startY) then <.div(^.cls := "start", "S")
            else if isWall then <.div(^.cls := "lava")
            else if onFoundPath then
              <.div(^.cls := (if isBramble then "path found bramble" else "path found"))
            else if onActivePath then
              if isActiveHead then <.div(^.cls := (if isBramble then "path active bramble" else "path active"), bestCost(pos).toString)
              else <.div(^.cls := (if isBramble then "path bramble" else "path"), bestCost(pos).toString)
            else if bestCost.contains(pos) then
              <.div(^.cls := (if isBramble then "visited bramble" else "visited"), bestCost(pos).toString)
            else if isBramble then <.div(^.cls := "bramble")
            else <.div(^.cls := "floor")
          )
        }
      )
    ),
    <.div(^.cls := "btn-group",
      <.button(^.cls := "btn btn-outline-secondary", ^.onClick --> { reset(); rerender() }, "Reset"),
      <.button(^.cls := "btn btn-outline-primary", ^.onClick --> { step(); rerender() }, "Step")
    )
  )
}
