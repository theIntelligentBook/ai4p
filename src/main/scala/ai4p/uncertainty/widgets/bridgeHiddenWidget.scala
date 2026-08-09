package ai4p.uncertainty.widgets

import com.wbillingsley.veautiful.html.{<, DHtmlComponent, ^}
import org.scalajs.dom

import scala.util.Random

import ai4p.{*, given}
import Common.*

import site.given

/**
 * The same game as [[BridgeOpenWidget]], but seen from one player's seat: only that player's own
 * hand and their partner's (the "dummy", tabled after the opening lead — standard bridge
 * convention) are shown face-up. The other side's hands are drawn as unknown cards.
 *
 * The widget still holds the real deal internally — the hidden opponents play their true cards,
 * exactly as they would across a real table, via a plain (cheap) full-information minimax solve.
 * When `showAiPanels` is true (the default), *choosing* a card for the visible side is instead
 * offered up by [[MonteCarloBridgeBot]]: it repeatedly guesses a full deal consistent with what's
 * been played and shown void so far, solves each guess exactly, and averages the results. That
 * average is also what drives the "how likely is the missing Queen on this side?" style
 * probabilities shown below the hidden hands.
 *
 * Set `showAiPanels = false` for a slide that's just introducing what "hidden hands" looks like,
 * before the Monte Carlo bot has been explained — the moves/probability panels are hidden, and
 * (since there's nothing expensive left to defer) manual play works immediately rather than
 * waiting on a "Play" button. With `showAiPanels = true`, the Monte Carlo panels only ever get
 * computed once "Play" is clicked — some hands take a couple of seconds to work through, which
 * would otherwise stall the page as soon as the deck containing this widget was opened.
 */
case class BridgeHiddenWidget(
  cardsPerHand: Int = 5,
  trump: Option[Suit] = Some(Suit.Spades),
  perspective: Seat = Seat.South,
  /** Fixes any seat named here to a specific hand (see [[Card.parseHand]]) instead of dealing it randomly — for setting up a known teaching position. */
  fixedHands: Map[Seat, String] = Map.empty,
  /** Whether to show the Monte Carlo bot's ranked move suggestions and "where's the missing card?" probabilities. */
  showAiPanels: Boolean = true
) extends DHtmlComponent {

  val visibleSeats: Set[Seat] = Set(perspective, perspective.partner)
  val hiddenSeats: Vector[Seat] = Seat.all.filterNot(visibleSeats.contains)

  def freshDeal(): PlayState = PlayState(Deal.custom(cardsPerHand, fixedHands), trump, Seat.North)

  var play: PlayState = freshDeal()
  var playing: Boolean = !showAiPanels
  var candidateMoves: Vector[(Card, Double)] = Vector.empty
  var cardProbs: Map[Card, Map[Seat, Double]] = Map.empty
  var autoRunning = false
  var timerId: Option[Int] = None

  def isOver: Boolean = play.deal.handOf(play.toPlay).isEmpty && play.currentTrick.isEmpty
  def isVisibleTurn: Boolean = visibleSeats.contains(play.toPlay)

  def refresh(): Unit =
    if !showAiPanels || !playing || isOver then
      candidateMoves = Vector.empty
      cardProbs = Map.empty
    else
      candidateMoves = if isVisibleTurn then MonteCarloBridgeBot.rankMoves(play, visibleSeats, samples = 50) else Vector.empty
      cardProbs = MonteCarloBridgeBot.cardLocationProbabilities(play, hiddenSeats.toSet, samples = 200)

  /** The hidden opponents' hands are only hidden from the *viewer* — the widget knows their real cards, and auto-plays for them (via a plain, cheap minimax solve, not the Monte Carlo bot), just as an opponent across a real table would. */
  def playHiddenTurns(): Unit =
    while !isOver && !isVisibleTurn do
      val best = BridgeSolver.rankMoves(play).headOption
      best.foreach((card, _) => play = play.play(card))

  def startPlaying(): Unit =
    playing = true
    playHiddenTurns()
    refresh()
    rerender()

  def playCard(card: Card): Unit =
    play = play.play(card)
    playHiddenTurns()
    refresh()
    rerender()

  def newDeal(): Unit =
    stopAuto()
    play = freshDeal()
    playing = !showAiPanels
    playHiddenTurns()
    candidateMoves = Vector.empty
    cardProbs = Map.empty
    rerender()

  def stopAuto(): Unit =
    timerId.foreach(dom.window.clearInterval(_))
    timerId = None
    autoRunning = false

  def toggleAuto(): Unit =
    if autoRunning then
      stopAuto()
      rerender()
    else
      autoRunning = true
      timerId = Some(dom.window.setInterval(() => {
        if isOver then stopAuto()
        else candidateMoves.headOption.foreach((card, _) => playCard(card))
      }, 1200))
      rerender()

  override def afterDetach(): Unit = stopAuto()

  playHiddenTurns()

  override protected def render =
    <.div(^.cls := BridgeCommon.styling.className,
      <.p(^.cls := "br-label",
        s"Trump: ${trump.map(t => s"${t.char} ${t.toString}").getOrElse("No Trump")}   —   $cardsPerHand cards per hand   —   " +
          s"seeing $perspective's hand and dummy (${perspective.partner}); ${hiddenSeats.mkString(" & ")} hidden"
      ),

      {
        def seatHand(seat: Seat) =
          val hand = play.deal.handOf(seat)
          val voids = play.knownVoids.getOrElse(seat, Set.empty)
          <.div(
            BridgeCommon.handRow(
              seat, isTurn = !isOver && play.toPlay == seat,
              hand = if visibleSeats.contains(seat) then Some(hand) else None, hiddenCount = hand.size,
              playable = card => playing && isVisibleTurn && !isOver && play.toPlay == seat && play.legalPlays(seat).contains(card),
              onPlay = card => playCard(card)
            ),
            if !visibleSeats.contains(seat) && voids.nonEmpty then
              <.span(^.cls := "br-void-note", s"(known void in ${voids.map(_.char).mkString(" ")})")
            else <.span()
          )
        BridgeCommon.tableLayout(
          BridgeCommon.compassLayout(
            north = seatHand(Seat.North), east = seatHand(Seat.East),
            south = seatHand(Seat.South), west = seatHand(Seat.West),
            center = BridgeCommon.trickRow(play.currentTrick)
          ),
          BridgeCommon.trickHistory(play.history, trump)
        )
      },

      BridgeCommon.scoreRow(play.tricksWon),

      if isOver then <.p(^.cls := "br-label", "Hand over.")
      else if !showAiPanels then <.p(^.cls := "br-label", s"Click a card above to play.")
      else if !playing then
        <.p(^.cls := "br-label", "Click Play to let the Monte Carlo bot take over from here.")
      else if isVisibleTurn then
        <.div(^.cls := "br-moves",
          <.div(^.cls := "br-section-title", s"Monte Carlo top moves for $perspective (averaged over sampled hidden hands)"),
          for (card, score) <- candidateMoves.take(5) yield
            BridgeCommon.moveButton(card, f"${card.prettyString}  →  avg $score%.2f tricks for ${if perspective.isNS then "N/S" else "E/W"}", playCard(card))
        )
      else <.p(^.cls := "br-label", s"${play.toPlay} (hidden) to play..."),

      if cardProbs.nonEmpty then
        <.div(^.cls := "br-probs",
          <.div(^.cls := "br-section-title", "Where are the missing cards?"),
          for card <- Card.sortHand(cardProbs.keys.toSeq) yield
            BridgeCommon.probRow(card, cardProbs(card), hiddenSeats)
        )
      else <.span(),

      <.div(^.cls := "br-controls",
        if !showAiPanels then <.span()
        else if !playing then
          <.button(^.cls := "btn btn-primary btn-sm", ^.onClick --> startPlaying(), "▶ Play")
        else
          <.button(^.cls := (if autoRunning then "btn btn-warning btn-sm" else "btn btn-outline-success btn-sm"),
            ^.onClick --> toggleAuto(), if autoRunning then "Stop" else "Auto"),
        <.button(^.cls := "btn btn-outline-secondary btn-sm", ^.onClick --> newDeal(), "New deal")
      )
    )
}
