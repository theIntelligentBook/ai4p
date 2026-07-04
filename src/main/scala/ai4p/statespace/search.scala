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
       |---
       |
       |## Flood-fill
       |
       |Imagine we were pouring treacle onto the goal square. Think about where that treacle would flow.
       |
       |The direction it reaches us from first is the shortest path.
       |
       |""".stripMargin)
   .veautifulSlide(<.div(
    <.h2("Algorithms on a maze of paths"),
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
  .veautifulSlide(<.div(
    <.h2("Algorithms in a mostly empty maze"),
    <.p("(or, as I like to call it, jelly flood)"),
    JellyFlood(
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
                        |................""".stripMargin)
  ))
  .markdownSlides(
    """|
       |## Breadth-first search
       |
       |* Works like flood-fill, but from the start instead of the goal.
       |
       |* It gives you the distance to every square, but you have to remember the path as you go
       |
       |* (Just remembering the distance won't tell you which way to go at corners)
       |
       |""".stripMargin)
   .veautifulSlide(<.div(
    <.h2("Algorithms on a maze of paths"),
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
    <.h2("Algorithms in a mostly empty maze"),
    <.p("(or, as I like to call it, jelly flood)"),
    JellyFlood(
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
                        |................""".stripMargin)
  ))
  .markdownSlides(
    """|
       |## Depth-first search
       |
       |* Works like flood-fill, but from the start instead of the goal.
       |
       |* It gives you the distance to every square, but you have to remember the path as you go
       |
       |* (Just remembering the distance won't tell you which way to go at corners)
       |
       |""".stripMargin)
   .veautifulSlide(<.div(
    <.h2("Algorithms on a maze of paths"),
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
  .veautifulSlide(<.div(
    <.h2("Algorithms in a mostly empty maze"),
    <.p("(or, as I like to call it, jelly flood)"),
    JellyFlood(
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
                        |................""".stripMargin)
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
    <.h2("Algorithms on a maze of paths"),
    JellyFlood(
        w = 16, h = 16, startX=1, startY=1, goalX=8, goalY=8, 
        mazeString = """|###############
                        |#..............
                        |#.############.
                        |#.#............
                        |#...#########..
                        |#*###.......##.
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
  .veautifulSlide(<.div(
    <.h2("Algorithms in a mostly empty maze"),
    <.p("(or, as I like to call it, jelly flood)"),
    JellyFlood(
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
                        |................""".stripMargin)
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
    <.h2("Algorithms on a maze of paths"),
    JellyFlood(
        w = 16, h = 16, startX=1, startY=1, goalX=8, goalY=8, 
        mazeString = """|###############
                        |#..............
                        |#.############.
                        |#.#...*****....
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
                        |#..............""".stripMargin)
  ))    
  .veautifulSlide(<.div(
    <.h2("Algorithms in a mostly empty maze"),
    <.p("(or, as I like to call it, jelly flood)"),
    JellyFlood(
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
                        |................""".stripMargin)
  ))
  .markdownSlide(willCcBy)
  .renderSlides
