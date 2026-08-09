package ai4p.uncertainty
import com.wbillingsley.veautiful.html.*
import com.wbillingsley.veautiful.doctacular.DeckBuilder

import <._
import ^._

import ai4p.{*, given}
import Common.*
import ai4p.uncertainty.widgets.{BridgeOpenWidget, BridgeHiddenWidget, ParticleFilter1D, ParticleFilter2D, Suit, Seat}

import site.given


val monteCarlo = DeckBuilder(1920, 1080)
  .markdownSlide(
    """
      |# Monte Carlo Methods
      |
      |""".stripMargin
  ).withClass("center middle")
  .markdownSlides(
    """
      |## Let's play Bridge
      |
      |Card games are turn-based, but they're often games of *incomplete information*: you have to
      |work out your best move even though you don't know exactly where all the cards are.
      |
      |Over time, as play continues, your opponents' moves tell you more about what the true state of
      |the game actually is — so you're simultaneously *learning* and *deciding*.
      |
      |First, let's show you the game (slightly reduced) in a version where all the hands are visible
      |
      |""".stripMargin
  )
  .veautifulSlide(<.div(
    <.h2("Bridge"),
    markdown.div(
      """|Bridge is played in pairs. North/South are a team, so are East/West
         |
         |It's played in turns, going clockwise. You have to "follow suit" if you can.
         |
         |Higher cards win. If you're out of that suit, though:
         |* If you play a "trump" (whichever suit is trumps that round), it beats any card of the usual suit
         |* Any other card is just discarded (can't win)
         |
         |The team that nominated the trumps (via bidding that we won't get into) is trying to make a certain
         |number of "tricks"
         |
         |""".stripMargin
    ),
    <.p(BridgeOpenWidget(cardsPerHand = 5, trump = Some(Suit.Spades)))
  ))
  .veautifulSlide(<.div(
    <.h2("Playing with two hands hidden"),
    markdown.div(
      """|The team that's trying to make the contract can see both their hands (actually after the first play, but never mind)
         |
         |They can't see the opposing team's cards. So, from their perspective the game's state is not fully known -
         |it's a game of *incomplete information*
         |
         |But as their opponents play, it's going to tell them things about what their opponents are holding.
         |""".stripMargin
    ),
    // This slide is just introducing what "hidden hands" looks like — the Monte Carlo bot hasn't
    // been explained yet, so its move suggestions and card-location probabilities stay switched
    // off here (they show up on the later "Playing with two hands hidden" slide instead).
    <.p(BridgeHiddenWidget(
      cardsPerHand = 5, trump = Some(Suit.Spades), perspective = Seat.South,
      fixedHands = Map(
        Seat.North -> "AQ.QJT.-.-",
        Seat.East  -> "KJT.AK.K.-",
        Seat.South -> "T.-.AQJ.T",
        Seat.West  -> "-.-.T.AKQJ"
      ),
      showAiPanels = false
    ))
  ))
  .markdownSlides(
    """
      |## Monte Carlo 
      |
      |"Monte Carlo" is the name of a city with a famous casino. 
      |
      |Monte Carlo methods are basically
      |
      |* generate a lot of random scenarios that meet what we know about the problem
      |* see how they perform
      |
      |We don't really know where the cards are to work out the best move, but "most often"
      |(from trying a bunch) this seems like the best move.
      |
      |""".stripMargin
  )
  .veautifulSlide(<.div(
    <.h2("Double dummy: minimax with everything visible"),
    markdown.div(
      """|
         |Let's go back to the version with everything visible, which Bridge players call 
         |"double dummy" (and is a good way of teaching the game)
         |
         |For a small 5-card hand, this is solvable with minimax and it'll play out a bit like
         |our noughts and crosses solver back in "small games"
         |""".stripMargin
    ),
    <.p(BridgeOpenWidget(cardsPerHand = 5, trump = Some(Suit.Spades)))
  ))
  .markdownSlides(
    """
      |## Now hide two of the hands
      |
      |In a real game, East and West's hands aren't on the table — only your own hand and dummy's are.
      |Minimax alone can't run anymore: it needs to know every card to search the game tree.
      |
      |But we still know *something* about the hidden hands:
      |
      |* Exactly how many cards each opponent is holding
      |* Every card that's already been played (once a card is played, everyone at the table sees it)
      |* Any suit an opponent has already shown they're out of, by failing to follow suit
      |
      |But we're doing this on a computer. We can generate *random* hands that meet those criteria.
      |
      |For each hand we generate, minimax can play the game, and we can see statistically which moves
      |*tend* to win how many tricks.
      |
      |---
      |
      |## The Monte Carlo bridge bot
      |
      |Generate a number of random deals that match what we know so far. 
      |Each random deal we generate is called a *determinization*.
      |
      |For each one:
      |
      |1. Every hand is now fully known, so solve it exactly with the same minimax as before
      |2. Record which card came out best in *that* guess
      |
      |We'll generate a few dozen of them, and this can tell us two things:
      |
      |* How well each move performed on average in minimax simulation
      |* For any specific missing card (say, K♣), how often it was in each of our opponents' hands
      |
      |""".stripMargin
  )
  .veautifulSlide(<.div(
    <.h2("Playing with two hands hidden"),
    markdown.div(
      """|We're playing North/South and the computer is playing East/West. It'll show us how its
         |random hands performed.
         |""".stripMargin
    ),
    <.p(BridgeHiddenWidget(cardsPerHand = 5, trump = Some(Suit.Spades), perspective = Seat.South))
  ))
  .markdownSlides(
    """
      |## Why this matters
      |
      |The situation where we have to guess at unknown states is pretty common.
      |
      |If we were to try to work it out like an equation (how likely is it East has the King), the
      |equation would start getting hard as we add in different kinds of observations.
      |
      |But randomly generating a population of possibilities has a more stable cost. It's not too 
      |complex, there's just a lot of them, but we can scale the number up or down.
      |
      |Some other examples:
      |
      |* **Monte Carlo Tree Search** (the technique behind AlphaGo) — instead of guessing hidden
      |  *cards*, it randomly simulates ("rolls out") the rest of a game to estimate how good a move is
      |* **Monte Carlo integration** — approximating an area, volume, or expected value that's too
      |  complex to work out with calculus, just by sampling random points
      |
      |And our next example, helping robots know where they are...
      |
      |""".stripMargin
  )
  .markdownSlide(willCcBy)
  .renderSlides
