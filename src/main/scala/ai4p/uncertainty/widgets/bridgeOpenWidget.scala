package ai4p.uncertainty.widgets

import com.wbillingsley.veautiful.html.{<, DHtmlComponent, ^}
import org.scalajs.dom

import scala.util.Random

import ai4p.{*, given}
import Common.*

import site.given

/**
 * "Double dummy" bridge: all four hands are dealt face-up, and every move is chosen by the exact
 * minimax solver in [[BridgeSolver]] — nothing is hidden, so there's no need to guess. Click a
 * card to override the bot and try your own line, or let it play a full hand out on its own.
 *
 * You can always play a card yourself, whether or not the AI is switched on — the game rules
 * (following suit, trumping, trick-winning) are just plain logic and don't need any solving.
 * Minimax only ever runs once the "Play" button is clicked, though: some hands take a couple of
 * seconds to solve, which would otherwise stall the page as soon as the deck containing this
 * widget was opened, and isn't needed just to demonstrate how a hand of bridge is played.
 */
case class BridgeOpenWidget(
  cardsPerHand: Int = 5,
  trump: Option[Suit] = Some(Suit.Spades),
  /** Fixes any seat named here to a specific hand (see [[Card.parseHand]]) instead of dealing it randomly — for setting up a known teaching position. */
  fixedHands: Map[Seat, String] = Map.empty
) extends DHtmlComponent {

  def freshDeal(): PlayState = PlayState(Deal.custom(cardsPerHand, fixedHands), trump, Seat.North)

  var play: PlayState = freshDeal()
  var playing: Boolean = false
  var rankedMoves: Vector[(Card, Int)] = Vector.empty
  var autoRunning = false
  var timerId: Option[Int] = None

  def isOver: Boolean = play.deal.handOf(play.toPlay).isEmpty && play.currentTrick.isEmpty

  def refreshRanking(): Unit =
    rankedMoves = if !playing || isOver then Vector.empty else BridgeSolver.rankMoves(play)

  def startPlaying(): Unit =
    playing = true
    refreshRanking()
    rerender()

  def playCard(card: Card): Unit =
    play = play.play(card)
    refreshRanking()
    rerender()

  def playBest(): Unit =
    rankedMoves.headOption.foreach((card, _) => playCard(card))

  def newDeal(): Unit =
    stopAuto()
    play = freshDeal()
    playing = false
    rankedMoves = Vector.empty
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
        if isOver then stopAuto() else playBest()
      }, 900))
      rerender()

  override def afterDetach(): Unit = stopAuto()

  override protected def render =
    val (ns, ew) = play.tricksWon
    <.div(^.cls := BridgeCommon.styling.className,
      <.p(^.cls := "br-label",
        s"Trump: ${trump.map(t => s"${t.char} ${t.toString}").getOrElse("No Trump")}   —   $cardsPerHand cards per hand   —   all four hands visible"
      ),

      {
        def seatHand(seat: Seat) = BridgeCommon.handRow(
          seat, isTurn = !isOver && play.toPlay == seat,
          hand = Some(play.deal.handOf(seat)), hiddenCount = 0,
          playable = card => !isOver && play.toPlay == seat && play.legalPlays(seat).contains(card),
          onPlay = card => playCard(card)
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
      else if !playing then
        <.p(^.cls := "br-label", "Play any card yourself, or click ▶ Play to let the minimax solver take over.")
      else <.div(^.cls := "br-moves",
        <.div(^.cls := "br-section-title", s"Minimax-ranked moves for ${play.toPlay}"),
        for (card, score) <- rankedMoves.take(5) yield
          BridgeCommon.moveButton(card, s"${card.prettyString}  →  ${score} tricks for ${if play.toPlay.isNS then "N/S" else "E/W"}", playCard(card))
      ),

      <.div(^.cls := "br-controls",
        if !playing then
          <.button(^.cls := "btn btn-primary btn-sm", ^.onClick --> startPlaying(), "▶ Play")
        else <.span(
          <.button(^.cls := "btn btn-outline-primary btn-sm", ^.onClick --> playBest(), "Play best"),
          <.button(^.cls := (if autoRunning then "btn btn-warning btn-sm" else "btn btn-outline-success btn-sm"),
            ^.onClick --> toggleAuto(), if autoRunning then "Stop" else "Auto")
        ),
        <.button(^.cls := "btn btn-outline-secondary btn-sm", ^.onClick --> newDeal(), "New deal")
      )
    )
}
