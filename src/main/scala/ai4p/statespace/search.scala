package ai4p.statespace
import com.wbillingsley.veautiful.html.{<, ^, unique}
import com.wbillingsley.veautiful.doctacular.DeckBuilder

import <._
import ^._

import ai4p.{*, given}
import Common.*

import willtap.imperativeTopic.{JellyFlood, SearchGrid}

import widgets.*

val searchStrategies = DeckBuilder(1920, 1080) 
  .markdownSlide(
    """
      |# Search strategies
      |
      |""".stripMargin
  ).withClass("center middle")
  .imageSlide("Euromicro 1981 poster, by Ros Billingsley", "images/micromouse poster.jpeg")
  .imageSlide("John Billingsley at a Micromouse contest", "images/micromouse john.jpg")
  .imageSlide("Micromouse maze from above", "images/micromouse above.jpg")
  .markdownSlides(
    """|
       |## Micromouse
       |
       |* Longstanding competition to get a robot to explore a maze and find the fastest path to the middle
       |
       |* These days, fastest doesn't mean shortest, but let's stick with the 1980s for now
       |
       |* If we just follow the left hand wall, we can get stuck in a loop
       |
       |We need a systematic way of exploring the squares of the maze to find which ones lead to the miidle.
       |Which way do we go first?
       |
       |""".stripMargin)
   .veautifulSlide(<.div(
    <.h2("Following the left-hand wall won't work"),
    JellyFlood(
        w = 16, h = 16, startX=1, startY=1, goalX=8, goalY=8, 
        mazeString = """|###############
                        |#..............
                        |#.############.
                        |#.#............
                        |#...#########..
                        |#.###.......##.
                        |#.#...####..#..
                        |#.######....#.#
                        |#.....#..##.#..
                        |#.#.#.#..#..##.
                        |#.#########..#.
                        |#.....#.....##.
                        |#.#####.###..#.
                        |#..#.#..#.#.##.
                        |#..............""".stripMargin)
  )) 
  .markdownSlides(
    """|
       |## Breadth-first search
       |
       |* At each square, we have to consdier its neighbours
       |
       |* And their neighbours...
       |
       |Breadth first search explores each path to the same distance, expanding step by step
       |
       |""".stripMargin)
   .veautifulSlide(<.div(
    <.h2("Breadth First Search"),
        SearchGrid(
            w = 16, h = 16, startX=1, startY=1, goalX=8, goalY=8, 
            mazeString = """|###############
                            |#..............
                            |#.############.
                            |#.#............
                            |#...#########..
                            |#.###.......##.
                            |#.#...####..#..
                            |#.######....#.#
                            |#.....#..##.#..
                            |#.#.#.#..#..##.
                            |#.#########..#.
                            |#.....#.....##.
                            |#.#####.###..#.
                            |#..#.#..#.#.##.
                            |#..............""".stripMargin,
            algorithm=SearchGrid.Algorithm.BFS
    ))
   )    
  .veautifulSlide(<.div(
    <.h2("Breadth First Search"),
    SearchGrid(
        w = 15, h = 15, startX=5, startY=5, goalX=14, goalY=14, 
        mazeString = """|................
                        |................
                        |................
                        |...........#....
                        |...........#....
                        |...........#....
                        |..##########....
                        |................
                        |................
                        |................
                        |................
                        |................
                        |................
                        |................
                        |................""".stripMargin, SearchGrid.Algorithm.BFS))
  )
  .markdownSlides(
    """|
       |## Depth-first search
       |
       |Depth First Search pursues each possibility to exhaustion before moving on to the next one
       |
       |
       |""".stripMargin)
   .veautifulSlide(<.div(
    <.h2("Depth First Searchs"),
    SearchGrid(
        w = 16, h = 16, startX=1, startY=1, goalX=8, goalY=8, 
        mazeString = """|###############
                        |#..............
                        |#.############.
                        |#.#............
                        |#...#########..
                        |#.###.......##.
                        |#.#...####..#..
                        |#.######....#.#
                        |#.....#..##.#..
                        |#.#.#.#..#..##.
                        |#.#########..#.
                        |#.....#.....##.
                        |#.#####.###..#.
                        |#..#.#..#.#.##.
                        |#..............""".stripMargin, SearchGrid.Algorithm.DFS)
  ))    
  .veautifulSlide(<.div(
    <.h2("Depth First Search"),
    SearchGrid(
        w = 15, h = 15, startX=5, startY=5, goalX=14, goalY=14, 
        mazeString = """|................
                        |................
                        |................
                        |...........#....
                        |...........#....
                        |...........#....
                        |..##########....
                        |................
                        |................
                        |................
                        |................
                        |................
                        |................
                        |................
                        |................""".stripMargin, SearchGrid.Algorithm.DFS)
  ))
  .markdownSlides(
    """|
       |## Prioritisation
       |
       |Sometimes, we'll want to take a guess as to which the best option to look at next is.
       |
       |Dijkstra's algorithm picks decides at every step to consider the *lowest cost so far*. 
       |Because all our squares cost 1 movement point, it works a bit like breadth-first search.
       |
       |So, we've introduced a "bramble" square that you can pass through but only slowly to see the difference
       |
       |""".stripMargin)
    .veautifulSlide(<.div(
    <.h2("Dijkstra's Algorithm"),
    SearchGrid(
        w = 16, h = 16, startX=1, startY=1, goalX=8, goalY=8, 
        mazeString = """|###############
                        |#.**...........
                        |#.############.
                        |#.#............
                        |#...#########..
                        |#*###.......##.
                        |#*#...####..#..
                        |#*######....#.#
                        |#.....#..##.#..
                        |#.#.#.#..#..##.
                        |#.#########..#.
                        |#.....#.....##.
                        |#.#####.###..#.
                        |#..#.#..#.#.##.
                        |#..............""".stripMargin, SearchGrid.Algorithm.Dijkstra)
  ))    
  .veautifulSlide(<.div(
    <.h2("Dijkstra's Algorithm"),
    SearchGrid(
        w = 15, h = 15, startX=5, startY=5, goalX=14, goalY=14, 
        mazeString = """|........***....
                        |........***....
                        |........***....
                        |........***....
                        |........***....
                        |........***....
                        |........***....
                        |........***....
                        |........***....
                        |........***....
                        |........***....
                        |........***....
                        |........***....
                        |........***....
                        |...............""".stripMargin, SearchGrid.Algorithm.Dijkstra)
  ))
  .markdownSlides(
    """|
       |## Greedy Breadth First Search
       |
       |Greedy Breadth First search uses a heuristic to guess the remaining cost, and
       |picks paths that are closest to the goal
       |
       |""".stripMargin)
    .veautifulSlide(<.div(
    <.h2("Greedy Breadth First Search"),
    SearchGrid(
        w = 16, h = 16, startX=1, startY=1, goalX=8, goalY=8, 
        mazeString = """|###############
                        |#.**...........
                        |#.############.
                        |#.#............
                        |#...#########..
                        |#*###.......##.
                        |#*#...####..#..
                        |#*######....#.#
                        |#.....#..##.#..
                        |#.#.#.#..#..##.
                        |#.#########..#.
                        |#.....#.....##.
                        |#.#####.###..#.
                        |#..#.#..#.#.##.
                        |#..............""".stripMargin, SearchGrid.Algorithm.GreedyBFS)
  ))    
  .veautifulSlide(<.div(
    <.h2("Greedy Breadth First Search"),
    SearchGrid(
        w = 15, h = 15, startX=5, startY=5, goalX=14, goalY=14, 
        mazeString = """|........***....
                        |........***....
                        |........***....
                        |........***....
                        |........***....
                        |........***....
                        |........***....
                        |........***....
                        |........***....
                        |........***....
                        |........***....
                        |........***....
                        |........***....
                        |........***....
                        |...............""".stripMargin, SearchGrid.Algorithm.GreedyBFS)
  ))
  .markdownSlides(
    """|
       |## A*
       |
       |Greedy Breadth First Search just used the *remaining* estimated cost.
       |
       |Dijkstra just used the *cost so far*
       |
       |The "A*" algorithm uses both. For each next step
       |* What's the cost so far
       |* What's the estimated remaining cost using a heuristic
       |Explore the option that has the lowest total.
       |
       |""".stripMargin)
   .veautifulSlide(<.div(
    <.h2("A*"),
    SearchGrid(
        w = 16, h = 16, startX=1, startY=1, goalX=8, goalY=8, 
        mazeString = """|###############
                        |#..............
                        |#.############.
                        |#.#..*****.....
                        |#...#########..
                        |#*###.......##.
                        |#*#...####..#..
                        |#*######....#.#
                        |#.....#..##.#..
                        |#.#.#.#..#..##.
                        |#.#########..#.
                        |#.....#.....##.
                        |#.#####.###..#.
                        |#..#.#..#.#.##.
                        |#..............""".stripMargin, SearchGrid.Algorithm.AStar)
  ))    
  .veautifulSlide(<.div(
    <.h2("*"),
    SearchGrid(
        w = 15, h = 15, startX=5, startY=5, goalX=14, goalY=14, 
        mazeString = """|...............
                        |...............
                        |...............
                        |...............
                        |...............
                        |...............
                        |.....*******...
                        |.....*******...
                        |.....*******...
                        |.....*******...
                        |...............
                        |...............
                        |...............
                        |...............
                        |...............""".stripMargin, SearchGrid.Algorithm.AStar)
  ))
  .markdownSlides(
    """|
       |## But we have a mouse...
       |
       |In Micromouse, though, we've got a robot inside the maze, exploring while it moves. 
       |
       |Travelling from one path to another would be expensive.
       |
       |So instead, most early mice used a simple algorithm called "flood fill" that also 
       |lets us talk about memoisation.
       |
       |Rather than remeber
       |
       |
       |""".stripMargin)
    .veautifulSlide(<.div(
        <.h2("A*"),
        SearchGrid(
            w = 16, h = 16, startX=1, startY=1, goalX=8, goalY=8, 
            mazeString = """|###############
                            |#..............
                            |#.############.
                            |#.#..*****.....
                            |#...#########..
                            |#*###.......##.
                            |#*#...####..#..
                            |#*######....#.#
                            |#.....#..##.#..
                            |#.#.#.#..#..##.
                            |#.#########..#.
                            |#.....#.....##.
                            |#.#####.###..#.
                            |#..#.#..#.#.##.
                            |#..............""".stripMargin, SearchGrid.Algorithm.AStar)
    ))          
    .markdownSlides(
    """|
       |## Flood-fill
       |
       |So instead, most early mice used a simple algorithm called "flood fill" that also 
       |lets us talk about memoisation.
       |
       |Rather than keep track of a set of paths, it'd keep track of how far it thinks every
       |square is from the goal.
       |
       |Imagine we were pouring treacle onto the goal square. Think about where that treacle would flow.
       |
       |""".stripMargin)
   .veautifulSlide(<.div(
    <.h2("Flood-fill"),
    JellyFlood(
        w = 16, h = 16, startX=1, startY=1, goalX=8, goalY=8, 
        mazeString = """|###############
                        |#..............
                        |#.############.
                        |#.#............
                        |#...#########..
                        |#.###.......##.
                        |#.#...####..#..
                        |#.######....#.#
                        |#.....#..##.#..
                        |#.#.#.#..#..##.
                        |#.#########..#.
                        |#.....#.....##.
                        |#.#####.###..#.
                        |#..#.#..#.#.##.
                        |#..............""".stripMargin)
  ))    
  .markdownSlide(willCcBy)
  .renderSlides
