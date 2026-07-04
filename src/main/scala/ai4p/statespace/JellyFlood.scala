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
    " .jelly-cell" -> s"width: ${w}px; height: ${w}px; border: 1px solid lightgray;",
    " .jelly-cell .floor" -> "background-color: #242424; width: 100%; height: 100%;",
    " .jelly-cell .lava" -> "background-color: darkred; width: 100%; height: 100%;",
    " .jelly-cell .jelly" -> s"background-color: darkgreen; width: 100%; height: 100%; border-radius: ${w/6}px; color: white; text-align: center; line-height: ${w}px;",
    " .jelly-cell .jelly.active" -> "animation: pulse-jelly 2s infinite;",
    " .jelly-cell .start" -> s"background-color: #3b82f6; width: 100%; height: 100%; border-radius: ${w/6}px; color: white; text-align: center; line-height: ${w}px;",
    " .jelly-cell .goal" -> s"background-color: #22c55e; width: 100%; height: 100%; border-radius: ${w/6}px; color: white; text-align: center; line-height: ${w}px;",
    " .jelly-cell .bramble" -> s"background-color: #7c4a1e; width: 100%; height: 100%;",
    " .jelly-cell .path" -> s"background-color: #7c4a1e; width: 100%; height: 100%;",
    " .jelly-cell .path.bramble" -> s"background-color: #7c4a1e; width: 100%; height: 100%;",
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
    case BFS, DFS, GreedyBFS, FloodFill, AStar, Dijkstra

  val algoLabel: Algorithm => String = {
    case Algorithm.FloodFill => "Flood Fill"
    case Algorithm.Dijkstra  => "Dijkstra"
    case Algorithm.BFS       => "BFS"
    case Algorithm.DFS       => "DFS"
    case Algorithm.GreedyBFS => "Greedy BFS"
    case Algorithm.AStar     => "A*"
  }

}

case class SearchGrid(w:Int=8, h:Int=8, startX:Int=0, startY:Int=0, goalX:Int = 7, goalY:Int = 7, mazeString:String = "", algorithm:SearchGrid.Algorithm) extends DHtmlComponent {

  val maze = mutable.Map.empty[(Int, Int), JellyFlood.Square]
  val paths = mutable.Queue.empty[Seq[(Int, Int)]]
  var activePath:Option[Seq[(Int, Int)]] = None
  var tick = 0
  
  // Contains the minimum distance we have seen so far for the square
  val distance = mutable.Map.empty[(Int, Int), Int]

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

  private def step():Unit = {
    // write this
  }

  def reset(): Unit = {
    tick = 0
    maze.clear()
    paths.clear()
    distance.clear()
    loadFromString(mazeString)
    paths.enqueue(Seq((startX, startY)))
    activePath = Some(paths.head)
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
            if activePath.contains((x, y)) then 
              if activePath.map(_.last).contains(x, y) then 
                <.div(^.cls := "jelly path active", d.toString)
              else 
                <.div(^.cls := "jelly path", d.toString)
            else 
              if distance.contains((x, y)) then 
                <.div(^.cls := "jelly", d.toString)
              else 
                maze.get((x, y)) match
                  case Some(Square.Empty) => <.div(^.cls := "floor")
                  case Some(Square.Bramble) => <.div(^.cls := "bramble")
                  case _ => <.div(^.cls := "lava")                
          )
        }
      )
    ),
    <.div(^.cls := "btn-group",
      <.button(^.cls := "btn btn-outline-secondary", ^.onClick --> { tick = 0; rerender() }, "Reset"),
      <.button(^.cls := "btn btn-outline-primary", ^.onClick --> {
        step()
        rerender()
      }, "Step")
    )
  )
}
