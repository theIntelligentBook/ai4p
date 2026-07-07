package ai4p.statespace

import com.wbillingsley.veautiful.html.*

import ai4p.{*, given}
import Common._
import Styles._

import scala.scalajs.js

val stateSpaceIntro = <.div(
  chapterHeading(1, "Search through a state space", "images/noughts and crosses.jpg"),
  marked("""
    |Let's start with the classical idea that our AI is trying to give us the best answer to something, out of a set of possibilites.
    |
    |In this topic we'll see
    |* AI in [small games](#/decks/smallGames/0), and how some games that we have to think about are small enough for a computer just to brute-force solve
    |* [Heuristics](#/decks/heuristics/0), and what we as humans tend to do when the game starts getting big
    |* The [search strategies](#/decks/searchStrategies/0) that computers use when the state starts getting bigger, when they might need to find a solution but not necessarily the best one.
    |
    |We'll also have a philosophical aside
    |* How AI is making the world become *pragmatic*
    |
    |""".stripMargin),
)
