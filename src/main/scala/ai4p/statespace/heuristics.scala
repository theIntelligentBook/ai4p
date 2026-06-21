package ai4p.statespace
import com.wbillingsley.veautiful.html.{<, ^, unique}
import com.wbillingsley.veautiful.doctacular.DeckBuilder

import <._
import ^._

import ai4p.{*, given}
import Common.*
import ai4p.statespace.widgets.MontyHall

val heuristics = DeckBuilder(1920, 1080) 
  .markdownSlide(
    """
      |# Heuristics
      |
      |""".stripMargin
  ).withClass("center middle")
  .markdownSlides(
    """
    |## Heuristics
    |
    |
    |So far, we've met a couple of very small games that already exceeded our human working memory (but not our computer's)
    |
    |Wo do manage to play these games though, so what is it we do?
    |
    |In the 1950s to 1970s, artificial intelligence was closely linked with "cognitive science" - understanding how humans think.
    |
    |Herb Simon proposed that humans often use *shortcut rules*, or "heuristics", to help them decide between alternatives.
    |
    |This also gave rise to the notion of "bounded rationality" - that people make decisions that *satisfice* (they deem the solution satisfactory), whether or not it is *optimal*.
    |
    |i.e. sometimes the heuristic won't be perfect.
    |
    |---
    |
    |## Cards of Doom, again
    |
    |Let's do Cards of Doom again, only this time I'm going to lay it out differently so your brain can identify a very good heuristic very much faster. 
    |
    |You're going to play first, but I'm only going to set out 12 cards. That's so you can see what the computer is doing to beat you every time (it's not doing minimax)
    |
    |Abd then you can do it to the computer.
    |
    |""".stripMargin
  )
  .veautifulSlide(<.div(
    <.h2("Cards of Doom with 12 cards"),
    CardsOfDoom(12, false, true)
  ))
  .veautifulSlide(<.div(
    <.h2("Cards of Doom with 27 cards"),
    CardsOfDoom(27, false, true)
  ))
  .markdownSlide("""
  |## So far 
  |
  |* When we look at a problem, we often use a heuristic to help us avoid having to think about too many states
  |
  |* Our brains obviously aren't just logic engines, because laying out the problem in a convenient manner helps us to 
  |  pick out a heuristic faster. In this case we've used some visual machinery and pattern matching too.
  |
  |The heuristics we come up with, though, aren't always going to be right. We'll talk about bias in machine learning next week
  |but Tversky and Kahneman in the 1970s hypothesised that there was a kind of "heuristic bias" that humans have.
  |
  |Let's look at a problem that's well known for making humans pick a faulty heuristic. 
  |""".stripMargin)
  .imageSlide("The Monty Hall Problem", "images/montyhall.jpg")
  .markdownSlide("""
  |## The Monty Hall Problem
  |
  |The Monty Hall problem comes from a tv game show *Let's Make a Deal* that was hosted by Monty Hall. 
  |(Though it never appeared exactly like this on the show)
  |
  |Contestants would be shown three doors. 
  |* One contains a fabulous prize (say, a car)
  |* The other two contain booby prizes (say, goats)
  |
  |The contestant choses a door.
  |
  |Monty Hall, the host, will then open one of the *other* doors, revealing a goat. (He always can, because there's two goats).
  |He then asks the contestant whether they would like to stick with their already-chosen door or switch to the other unrevealed door.
  |
  |Is it better to stick or switch?
  |""".stripMargin)
  .veautifulSlide(<.div(MontyHall.widget()))
  .markdownSlides("""
  |## The faulty heuristic
  |
  |When I told you that Monty can *always* open a door containing a goat, in a lot of people that will trigger them to adopt a fauly heuristic. Something like 
  |
  |> If he can *always* do it, then there's nothing special about the door he opens, so there's nothing special about the other door either. There's two unknown doors, so it's fifty-fifty
  |
  |If you try the simulation out, or enumerate the possibilites on paper, though, you'll find that switching succeeds 2/3 of the time, and sticking only succeeds 1/3 of the time.
  |
  |* 2/3 of the time, you initially picked a goat. <br > He was then forced to show you the other goat, so the remaining door must have the car.
  |* 1/3 of the time, you initially picked the car. <br > He can show you either goat, and the other door is also a goat.
  |
  |---
  |
  |## When things start getting big for AI too
  |
  |So far, the problems we've seen have been small enough for a computer to model *exhaustively*
  |
  |* The Prisoner's dilemma had 4 states
  |* Cards of Doom had *2n - 1* states
  |* Noughts and Crosses had 768 unique states
  |
  |But we can quickly start to get bigger than what a computer can exhaust too
  |
  |* Chess has somewhere between 10<sup>45</sup> and 10<sup>47</sup> possible board configurations
  |* There are 8 * 10<sup>67</sup> different ways of shuffling a deck of cards, so most card games have at least this many states
  |* Go has 2.08 * 10<sup>170</sup> possible board states
  |
  |---
  |
  |## Optimising our lookahead
  |
  |*Lookahead* was the process where we look at all the different positions that could come *after* we make a move, depending on what we do.
  |
  |If we can't look at the game exhaustively, looking at all the possibilites, we need to decide which ones to look at. 
  |
  |One of the more famous techniques for this is called *alpha-beta pruning*. The rule of thumb goes like this:
  |
  |* Suppose we're looking at a move and considering our opponent's replies
  |* Suppose we work out that they have a good reply, that makes this a bad move for us
  |* We can stop thinking about all their *other* replies to this move. We already know the move is bad.
  |
  |---
  |
  |## Alpha-beta pruning example
  |
  |Let's show a little example. Suppose we're only looking two "plies" ahead and this is what we've worked out so far
  |
  |<pre>
  |                [MAX]
  |               /     \
  |           [MIN]     [MIN]
  |           / \       / \
  |          3   5     2   ?
  |</pre>
  |
  |There's no point working out the value of the <code>?</code>. 
  |
  |We already know that for the move on the left, their best reply has score 3, but for the move on the right they have a move that'll only give us score 2.
  |
  |Regardless of the value of <code>?</code> we'd be better off making the move on the left.
  |
  |"Alpha-beta pruning" sounds fancy. But it's "alpha-beta" just because we can use those Greek letters for the players, and "pruning" just because it's about lopping off branches of the tree of possible move sequences.
  |
  |---
  |
  |## Computers use heuristics too
  |
  |Optimising our lookahead algorithm so we rule out bad moves early will help us look ahead *further* but not *forever*.
  |
  |Stockfish, which is the most popular Chess AI library (and can handily beat almost all humans) looks ahead an average of 15 to 20 full moves.
  |
  |Most games are longer than 20 full moves, so if it can't see to the end of the game, how does it know what's a good move or not?  
  |
  |Answer: it uses a heuristic!
  |
  |To a computer, a heuristic is some way of scoring a game state to know whether it's good or not.
  |
  |""".stripMargin)
  .imageSlide("Stockfish analysis of a chess game on lichess.org", "images/stockfish.png")
  .markdownSlides("""
  |## Heuristics for chess
  |
  |If you learnt chess as a child, you might have been taught this as a way to work out whether you are materially winning or losing:
  |
  |* Count a pawn as 1 point
  |* Count a knight or a bishop as 3 points
  |* Count a rook as 5 points
  |* Count a queen as 10 points
  |
  |The difference between your points and your opponent's points tells you if you are materially winning or losing
  |
  |If you were taught this as a child, you were taught a *heuristic* about how to value a chess position.
  |
  |---
  |
  |## Stockfish and heuristics
  |
  |Stockfish doesn't use a simple rules as a heuristic. Instead it uses a neural network (which we'll talk about in a later week)
  |
  |But it's still similar in that the neural network isn't *telling it the best move*, it's *telling it how to value the position*.
  |
  |The algorithm for playing the game is very much like what we've seen so far. Minimax with alpha-beta pruning.
  |
  |Though it has a few extra tricks up its sleeve:
  |
  |* There's an opening book so it knows some of the moves at the start-of the game off-by-heart
  |* When it gets to the end-game, it has some look-up tables, so it effectively knows those moves off-by-heart too
  |* It has some internal data structures ("transposition tables") that let it understand similar positions to ones it's seen before.
  |
  |---
  |
  |## Humans vs AI
  |
  |Here's a link to [the actual analysis of that game on lichess.org](https://lichess.org/p0Dt5kvE/black#92)
  |
  |For chess, AI is now so good that most chess sites will rate your accuracy in terms of a percentage. 
  |i.e. "How much *worse* than an AI are you?" is now the most common form of measurement.
  |
  |The scoring system on the right essentially uses the pawn measurement scale. -1.0 is one pawn down. 
  |
  |There are fractions of a point, though, because (via the neural network) it is considering your positional strength as well.
  |e.g. in the position I've linked to, white is only 2 pawns down, but he can't get the rook to the topmost rank to defend against
  |Ra1 checkmate, so the game is (effectively) lost to white.
  |""".stripMargin)
  .markdownSlide(willCcBy)
  .renderSlides
