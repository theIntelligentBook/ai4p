package ai4p.statespace.widgets

import com.wbillingsley.veautiful.html.{<, ^, DHtmlComponent, Styling}
import org.scalajs.dom.{Element, Node}
import scala.util.Random

import ai4p.{*, given}
import site.{*, given}

val montyHallStyle = Styling(
  """
    |display: block;
    |font-family: sans-serif;
    |max-width: 520px;
    |margin: 1em auto;
    |text-align: center;
  """.stripMargin
).modifiedBy(
  " .mh-doors"        -> "display: flex; gap: 1em; justify-content: center; margin: 1em 0;",
  " .mh-door"         -> "width: 120px; padding: 1em; border: 2px solid #888; border-radius: 8px; background: #f5f5f5; cursor: pointer; font-size: 1rem;",
  " .mh-door:disabled"-> "cursor: default;",
  " .mh-picked"       -> "border-color: #4285f4; background: #e8f0fe;",
  " .mh-revealed"     -> "border-color: #e67e22; background: #fef9e7;",
  " .mh-door-emoji"   -> "font-size: 2.5em;",
  " .mh-buttons"      -> "display: flex; gap: 1em; justify-content: center; margin: 0.5em 0;",
  " .btn-stay"        -> "padding: 0.5em 1.5em; border-radius: 6px; border: none; font-size: 1rem; cursor: pointer; background: #4285f4; color: white;",
  " .btn-switch"      -> "padding: 0.5em 1.5em; border-radius: 6px; border: none; font-size: 1rem; cursor: pointer; background: #34a853; color: white;",
  " .mh-stats"        -> "margin: 1.5em auto; border-collapse: collapse; width: 100%;",
  " .mh-stats th, .mh-stats td" -> "border: 1px solid #ccc; padding: 0.4em 0.8em;",
  " .mh-stats thead"  -> "background: #f0f0f0;"
).register()

/**
 * An interactive Monty Hall Problem widget.
 * The player picks a door, the host reveals a goat, then the player
 * can stay or switch. Tracks cumulative win rates for both strategies.
 */
object MontyHall {

  enum Phase:
    case Pick, Reveal, Result

  case class GameState(
    phase: Phase = Phase.Pick,
    doors: IndexedSeq[Boolean] = IndexedSeq.empty,  // true = car
    playerPick: Int = -1,
    hostReveal: Int = -1,
    switched: Boolean = false,
    won: Boolean = false,
    stayWins: Int = 0,
    switchWins: Int = 0,
    stayTotal: Int = 0,
    switchTotal: Int = 0
  )

  def freshDoors(): IndexedSeq[Boolean] =
    val car = Random.nextInt(3)
    IndexedSeq.tabulate(3)(_ == car)

  class MontyHallWidget extends DHtmlComponent {

    var state: GameState = GameState()

    def startGame(): Unit =
      state = state.copy(phase = Phase.Pick, doors = freshDoors(), playerPick = -1, hostReveal = -1)
      rerender()

    def pickDoor(i: Int): Unit =
      if state.phase != Phase.Pick then return
      // Host reveals a goat door that isn't the player's pick or the car
      val reveal = (0 until 3).find(d => d != i && !state.doors(d)).get
      state = state.copy(phase = Phase.Reveal, playerPick = i, hostReveal = reveal)
      rerender()

    def decide(switch: Boolean): Unit =
      if state.phase != Phase.Reveal then return
      val finalPick = if switch then
        (0 until 3).find(d => d != state.playerPick && d != state.hostReveal).get
      else state.playerPick
      val won = state.doors(finalPick)
      val newStayWins   = state.stayWins   + (if !switch && won then 1 else 0)
      val newSwitchWins = state.switchWins + (if  switch && won then 1 else 0)
      val newStayTotal   = state.stayTotal   + (if !switch then 1 else 0)
      val newSwitchTotal = state.switchTotal + (if  switch then 1 else 0)
      state = state.copy(
        phase = Phase.Result, switched = switch, won = won,
        stayWins = newStayWins, switchWins = newSwitchWins,
        stayTotal = newStayTotal, switchTotal = newSwitchTotal
      )
      rerender()

    def pct(wins: Int, total: Int): String =
      if total == 0 then "–" else f"${100.0 * wins / total}%.1f%%"

    def doorEmoji(i: Int): String =
      val s = state
      if s.phase == Phase.Result || (s.phase == Phase.Reveal && i == s.hostReveal) then
        if s.doors(i) then "🚗" else "🐐"
      else "🚪"

    def doorLabel(i: Int): String =
      val s = state
      if i == s.playerPick && s.phase != Phase.Pick then " ✋" else ""

    override protected def render =
      val s = state
      <.div(^.cls := montyHallStyle,
        <.h3("The Monty Hall Problem"),

        // Doors row
        <.div(^.cls := "mh-doors",
          for i <- 0 until 3 yield
            val clickable = s.phase == Phase.Pick
            <.button(
              ^.cls := s"mh-door${if i == s.playerPick then " mh-picked" else ""}${if i == s.hostReveal then " mh-revealed" else ""}",
              ^.attr("disabled") ?= (if !clickable then Some("true") else None),
              ^.onClick --> pickDoor(i),
              <.div(^.cls := "mh-door-emoji", doorEmoji(i)),
              <.div(^.cls := "mh-door-label", s"Door ${i + 1}${doorLabel(i)}")
            )
        ),

        // Phase-specific message and controls
        s.phase match
          case Phase.Pick =>
            <.p("Pick a door!")

          case Phase.Reveal =>
            <.div(
              <.p(s"The host opens Door ${s.hostReveal + 1} — it's a goat! Do you want to switch?"),
              <.div(^.cls := "mh-buttons",
                <.button(^.cls := "btn-stay",   ^.onClick --> decide(false), "Stay"),
                <.button(^.cls := "btn-switch",  ^.onClick --> decide(true),  "Switch")
              )
            )

          case Phase.Result =>
            <.div(
              <.p(if s.won then "🎉 You won the car!" else "🐐 You got a goat."),
              <.p(if s.switched then "You switched." else "You stayed."),
              <.button(^.onClick --> startGame(), "Play again")
            )
        ,

        // Running stats
        <.table(^.cls := "mh-stats",
          <.thead(<.tr(<.th("Strategy"), <.th("Wins"), <.th("Games"), <.th("Win rate"))),
          <.tbody(
            <.tr(<.td("Stay"),   <.td(s.stayWins.toString),   <.td(s.stayTotal.toString),   <.td(pct(s.stayWins,   s.stayTotal))),
            <.tr(<.td("Switch"), <.td(s.switchWins.toString), <.td(s.switchTotal.toString), <.td(pct(s.switchWins, s.switchTotal)))
          )
        )
      )
  }

  /** Factory — call this from a deck slide or challenge stage */
  def widget() =
    val w = new MontyHallWidget()
    w.startGame()
    w

}