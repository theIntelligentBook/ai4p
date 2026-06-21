package ai4p.statespace
import com.wbillingsley.veautiful.html.{<, ^, unique, Styling}
import com.wbillingsley.veautiful.doctacular.DeckBuilder

import <._
import ^._

import ai4p.{*, given}
import Common.*

import widgets.*
import site.given
import com.wbillingsley.veautiful.html.VHtmlBlueprint

val basicTable = Styling("""
|border-collapse: collapse;
|padding: 5px;
|""".stripMargin).modifiedBy(
  " th" -> "font-weight: bold; border: 1px solid black; padding: 0.6em;",
  " td" -> "border: 1px solid black;  padding: 0.6em;"
).register()

val smallgames = DeckBuilder(1920, 1080) 
  .markdownSlide(
    """
      |# Small Games
      |
      |
      |""".stripMargin
  ).withClass("center middle")
  .markdownSlides(
    """
    |## Small Games 
    |
    |Let's start with the idea that artificial intelligence is about how a machine can choose the best course of action
    |(or at least, a good course of action). 
    |
    |Apart from large language models, likely the first place most people have heard of AI is as an opponent in a game. 
    |
    |In this deck, we're going to look at deterministic games of complete information. That is:
    |
    |* The state of the game is completely known by both players, there's nothing hidden
    |* There's no randomness in the game at all
    |
    |
    |""".stripMargin
  )
  .imageSlide("The prisoners' dilemma", "images/prisonersdilemma.jpg")
  .veautifulSlide(<.div(
    <.h2("The Prisoners' Dilemma"),
    markdown.div("""
    |You'll be given the following sentences:
    |
    |* If you both stay silent, you'll both be sent to prison for a year
    |* If one of you stays silent and the other confesses, the confessor will go free and the one who stays silent will get 3 years
    |* If you both confess, you'll both get 2 years
    |""".stripMargin),
    <.table(^.cls := basicTable, 
      <.tr(<.td(), <.th("Silent"), <.th("Confess")),
      <.tr(<.th("Silent"), <.td("1 year, 1 year"), <.td("3 years, free")),
      <.tr(<.th("Confess"), <.td("free, 3 years"), <.td("2 years, 2 years")), 
    )
  ))
  .veautifulSlide(<.div(
    <.h2("The Prisoners' Dilemma"),
    <.table(^.cls := basicTable, 
      <.tr(<.th("Player 1"), <.td("Player 2")),
      <.tr(<.th("Silent"), <.table(
        <.tr(<.td("Silent"), <.td("Player 1: -1, Player 2: -1")),
        <.tr(<.td("Confess"), <.td("Player 1: -3, Player 2: 0")),
      )),
      <.tr(<.th("Confess"), <.table(
        <.tr(<.td("Silent"), <.td("Player 1: 0, Player 2: -3")),
        <.tr(<.td("Confess"), <.td("Payer 1: -2, Player 2: -2")),
      )),
    ),
    <.p("Let's rearrange it into a turn-based game so that one player goes first"),
  ))
  .veautifulSlide(<.div(
    <.h2("The Prisoners' Dilemma"),
    <.table(^.cls := basicTable, 
      <.tr(<.th("Player 1"), <.td("Player 2")),
      <.tr(<.th("Silent"), <.table(
        <.tr(<.td("Silent", <.br(), "Value: -1"), <.td("Player 1: -1, Player 2: -1")),
        <.tr(<.td("Confess", <.br(), "Value: -3"), <.td("Player 1: -3, Player 2: 0")),
      )),
      <.tr(<.th("Confess"), <.table(
        <.tr(<.td("Silent", <.br(), "Value: 0"), <.td("Player 1: 0, Player 2: -3")),
        <.tr(<.td("Confess", <.br(), "Value: -2"), <.td("Payer 1: -2, Player 2: -2")),
      )),
    ),
    <.p("Then let's score the game state by player 1's result, because we're looking from player 2's perspective"),
  ))
  .veautifulSlide(<.div(
    <.h2("The Prisoners' Dilemma"),
    <.table(^.cls := basicTable, 
      <.tr(<.th("Player 1"), <.td("Player 2")),
      <.tr(<.th("Silent", <.br(), "Value: -3"), <.table(
        <.tr(<.td("Silent", <.br(), "Value: -1"), <.td("Player 1: -1, Player 2: -1")),
        <.tr(<.td("Confess", <.br(), "Value: -3"), <.td("Player 1: -3, Player 2: 0")),
      )),
      <.tr(<.th("Confess", <.br(), "Value: -2"), <.table(
        <.tr(<.td("Silent", <.br(), "Value: 0"), <.td("Player 1: 0, Player 2: -3")),
        <.tr(<.td("Confess", <.br(), "Value: -2"), <.td("Payer 1: -2, Player 2: -2")),
      )),
    ),
    <.p("We can logically predict what player 2's decision should be, so we can score the initial choice by the result player 1 gets if player 2 plays its best next move"),
  ))
   .markdownSlides(
    """
    |## So far
    |
    |* The result of a game has a value
    |
    |* We want to ascribe each of our decisions a value, so we can know which is best
    |
    |* If the state space of the game can be fully explored, we can ascribe a value to *this move* based on the 
    |  value of the states that it leads to 
    |
    |* To do so, we assume our opponent will behave rationally too
    |
    |We are ascribing a value *now* to a game state based on what we expect to happen in the future. This is called *lookahead*.
    |
    |We do this linguistically too. *"You're dead if you make that move"* is said in present tense even though the
    |subsequent moves that lead to the loss happen in the future.
    |
    |""".stripMargin
  )
  .imageSlide("An aside: madman theory", "images/madman.jpg")
  .veautifulSlide(<.div(
    <.h2("An aside - 'madman theory'"),
    markdown.div("""
    |There is a theory that sometimes gets played in politics, ascribed to Richard Nixon, called "madman theory". That goes that if you
    |can persuade your opponent to *think* you're not playing rationally, you might be able to persuade *them* to make an suboptimal move.
    |
    |""".stripMargin),
    <.table(^.cls := basicTable, 
      <.tr(<.td(), <.th("Adversary: Capitulate"), <.th("Adversary: Hold out")),
      <.tr(<.th("Nixon: Stay at peace"), <.td("Nixon: +2, Adversary: +1"), <.td("Nixon: -3, Adversary: +3")),
      <.tr(<.th("Nixon: Go to War"), <.td("Nixon: -2, Adversary: +5"), <.td("Nixon: -10, Adversary -8")), 
    ),
    markdown.div("In this table, it's always rationally in Nixon's interest to stay at peace, but if he can make his adversary *think* he'll act irrationally and go to war if they don't capitulate, he can get a better result.")
  ))
  .markdownSlides(
    """
    |
    |## Cards of Doom
    |
    |Let's start making the state space very slightly bigger. 
    |
    |This is an outreach game I usually play with school children to show algorithms called "Cards of Doom"
    |
    |We're going to start with a single suit of cards. 13 cards. 
    |
    |On your turn, you can take 1, 2, or 3 cards. Whoever takes the last card (the card of doom) *loses*.
    |
    |
    |""".stripMargin
  )
  .veautifulSlide(<.div(
    <.h2("Cards of Doom with 13 cards"),
    <.p("You can play this in the deck too. Give it a go. It usually takes a while to work out a winning strategy"),
    CardsOfDoom(13)
  ))
  .veautifulSlide(<.div(
    markdown.div("""
    |## Cards of doom with 1 card 
    |
    |To analyse this one, let's play a variant with just one card and make you go first. 
    |
    |Bad luck, you've lost.
    |""".stripMargin),
    CardsOfDoom(1, false)
  ))
  .veautifulSlide(<.div(
    markdown.div("""
    |## Cards of Doom with 4 cards
    |
    |With 4 cards, there's a few moves available, but you can probably already see that only one of them wins
    |""".stripMargin),
    CardsOfDoom(4, false)
  ))
  .veautifulSlide(<.div(
    markdown.div("""
    |## Cards of Doom with 4 cards
    |
    |Intuitively, this game has very few states. How many cards there are, and whose turn it is. So in a game of Cards of Doom
    |with *n* cards, there's only *2n - 1* states to enumerate and we can just see which ones win or lose.
    |
    |Note that I'm always writing it as to whether the *first* player wins or loses.
    |""".stripMargin),
    {
      val cards = 4
      val myTurn = true

      val states = scala.collection.mutable.Buffer((cards, myTurn))

      val miniTabStyle = Styling("""
      | border-collapse: collapse;
      |""".stripMargin)
      .modifiedBy(
         " td,th" -> "padding-right: 5px;  vertical-align: top; ",
        " .take" -> "font-style: italic; font-size: 75%; color: darkgrey; border-right: 1px solid black; ",
        " .win" -> "color: green; font-weight: bold; font-size: 75%;",
        " .lose" -> "color: red; font-weight: bold; font-size: 75%;"
      ).register()

      def renderC(cards:Int, turn:Boolean):VHtmlBlueprint = 
        <.td(
          <.td(
            if cards > 1 then 
              s"$cards cards"
            else if cards > 0 then 
              "1 card"
            else if turn then "wins" else "loses"
          ),
          <.td(<.table(
            for i <- 1 to Math.min(cards, 3) yield <.tr(
              <.td(^.cls := "take", s"${if turn then "I" else "they"} take $i"), 
              renderC(cards - i, !turn)
            )
          ))
        )

      <.table(^.cls := miniTabStyle, renderC(4, true))
    },
  ))
  .veautifulSlide(<.div(
    markdown.div("""
    |## Minimax
    |
    |Let's change from "wins" and "loses" to 1 and 0. 
    |""".stripMargin),
    {
      val cards = 4
      val myTurn = true

      val states = scala.collection.mutable.Buffer((cards, myTurn))

      val miniTabStyle = Styling("""
      | border-collapse: collapse;
      |""".stripMargin)
      .modifiedBy(
         " td,th" -> "padding-right: 5px;  vertical-align: top; ",
        " .take" -> "font-style: italic; font-size: 75%; color: darkgrey; border-right: 1px solid black; ",
        " .win" -> "color: green; font-weight: bold; font-size: 75%;",
        " .lose" -> "color: red; font-weight: bold; font-size: 75%;"
      ).register()

      def renderC(cards:Int, turn:Boolean):VHtmlBlueprint = 
        <.td(
          <.td(
            if cards > 1 then 
              s"$cards cards"
            else if cards > 0 then 
              "1 card"
            else if turn then <.span(^.cls := "win", "1") else <.span(^.cls := "lose", "0")
          ),
          <.td(<.table(
            for i <- 1 to Math.min(cards, 3) yield <.tr(
              <.td(^.cls := "take", s"${if turn then "I" else "they"} take $i"), 
              renderC(cards - i, !turn)
            )
          ))
        )

      <.table(^.cls := miniTabStyle, renderC(4, true))
    },
    markdown.div("To calculate the value of a move we can use 'minimax'. Starting at the leaves, if it's the opponent's turn then take the *minimum* value from the children, and if it's our turn take the *maximum* value from the children")
  ))
  .veautifulSlide(<.div(
    markdown.div("""
    |## Let's do that for 13 cards
    |
    |It can quickly get bigger than we can think about, but small enough that a computer can do it in milliseconds
    |""".stripMargin),
    {
      val miniTabStyle = Styling("""
      | border-collapse: collapse;
      |
      |""".stripMargin)
      .modifiedBy(
         " td,th" -> "padding-right: 5px;  vertical-align: top; ",
        " .take" -> "font-style: italic; font-size: 75%; color: darkgrey; border-right: 1px solid black; ",
        " .win" -> "color: green; font-weight: bold; font-size: 75%;",
        " .lose" -> "color: red; font-weight: bold; font-size: 75%;"
      ).register()

      def renderC(cards:Int, turn:Boolean):VHtmlBlueprint = 
        <.td(
          <.td(
            if cards > 1 then 
              s"$cards cards"
            else if cards > 0 then 
              "1 card"
            else if turn then <.span(^.cls := "win", "1") else <.span(^.cls := "lose", "0")
          ),
          <.td(<.table(
            for i <- 1 to Math.min(cards, 3) yield <.tr(
              <.td(^.cls := "take", s"${if turn then "I" else "they"} take $i"), 
              renderC(cards - i, !turn)
            )
          ))
        )

      <.table(^.cls := miniTabStyle, renderC(13, true))
    },
  ))
  .veautifulSlide(<.div(
    <.h2("Cards of Doom with 13 cards"),
    <.p("You could try looking at the full chart of moves on the previous slide and manually working out which move is best, but let's face it, you're not a computer."),
    CardsOfDoom(13)
  ))
  .veautifulSlide(<.div(
    <.h2("Let's go bigger again"),
    <.p("Let's go bigger again. Noughts and crosses. The version on this slide is manually played by 2 players"),
    NoughtsAndCrosses(true)
  ))
  .markdownSlides("""
  |## Noughts and Crosses
  |
  |Noughts and crosses is probably big enough you don't think of trying to remember every possible state. It's still pretty small though.
  |
  |There's 9 squares, and 3 possible states: blank, nought, and cross. So that's only 3^9 = 19,683 possibilities
  |
  |Not all of those possibilites are legal. For example, you can't have two more crosses than you have noughts or the turn order's gone wrong.
  |
  |There's only:
  |* 5,478 possible unique positions
  |* 765 unique positions once you've taken accounted for symmetry, but
  |* 255,168 possible sequnces of moves
  |
  |Those are bigger than you and I can remember, but it's peanuts to a computer.
  |
  |The version on the next slide, you'll play against an AI that uses the minimax algorithm. It's not an advanced AI, just the algorithm I told you about, running in your browser.
  |It's almost instant and you'll never beat it.
  |
  |""".stripMargin)
  .veautifulSlide(<.div(
    <.h2("Noughts and Crosses vs minimax"),
    NoughtsAndCrossesAI(Player.Nought)
  ))
  .markdownSlide("""
  |## And it's not a lot of (generated) code
  |
  |<pre>
  |  // Minimax: returns (score, bestIndex)
  |  // score is from the perspective of `maximiser`
  |  def minimax(b: Board, current: Player, maximiser: Player, depth: Int): (Int, Int) =
  |    winner(b) match
  |      case Some(p) =>
  |        val score = if p == maximiser then 10 - depth else depth - 10
  |        (score, -1)
  |      case None if isDraw(b) => (0, -1)
  |      case None =>
  |        val moves = legalMoves(b)
  |        val scored = moves.map { idx =>
  |          val next = play(b, idx, current)
  |          val nextPlayer = if current == Player.Nought then Player.Cross else Player.Nought
  |          val (s, _) = minimax(next, nextPlayer, maximiser, depth + 1)
  |          (s, idx)
  |        }
  |        if current == maximiser then scored.maxBy(_._1)
  |        else scored.minBy(_._1)
  |</pre>
  |""".stripMargin)
  .markdownSlide(willCcBy)
  .renderSlides
