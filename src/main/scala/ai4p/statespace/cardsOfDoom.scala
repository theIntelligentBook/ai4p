package ai4p.statespace

import com.wbillingsley.veautiful.html.*
import com.wbillingsley.veautiful.templates.*

import scala.scalajs.js
import scala.util.Random

import com.wbillingsley.veautiful.doctacular.Challenge
import Challenge.Completion

import ai4p.{*, given} 
import site.given
import com.wbillingsley.veautiful.Morphing

val cardsOfDoomStyling = Styling(
  """|
     |""".stripMargin
).modifiedBy(
  " .cod-card" -> 
    """|    display: inline-flex;
       |    padding: 20px;
       |    margin: 10px;
       |    width: 80px;
       |    height: 100px;
       |    font-family: "Michroma", sans-serif;
       |    font-weight: bolder;
       |    text-align: center;
       |    border: 1px solid rebeccapurple;
       |    border-radius: 5px;
       |    background-color: #0d2d48;
       |    background-image: linear-gradient(white 2px, transparent 2px), linear-gradient(90deg, white 2px, transparent 2px), linear-gradient(rgba(255,255,255,.3) 1px, transparent 1px), linear-gradient(90deg, rgba(255,255,255,.3) 1px, transparent 1px);
       |    background-size: 100px 100px, 100px 100px, 20px 20px, 20px 20px;
       |    background-position: -2px -2px, -2px -2px, -1px -1px, -1px -1px;
       |    color: white;
       |    box-shadow: 10px 5px 5px #4e4d4d;
       |""".stripMargin,
  " .cod-card.cod-card-10" -> "background-color: #99461b;",
  " .cod-card.cod-card-11" -> "background-color: #99461b;",
  " .cod-card.cod-card-12" -> "background-color: #99461b;",
  " .cod-card.cod-card-0" -> "background-color: #64000c;",
).register()


val cardsOfDoomGridStyling = Styling(
  """|display: grid;
     |grid-template-rows: repeat(4, auto);
     |grid-auto-flow: column;
     |grid-auto-columns: max-content;
     |gap: 1rem;
     |align-items: start;
     |""".stripMargin
).modifiedBy(
  " .cod-card" -> 
    """|    display: inline-flex;
       |    padding: 20px;
       |    margin: 10px;
       |    width: 80px;
       |    height: 100px;
       |    font-family: "Michroma", sans-serif;
       |    font-weight: bolder;
       |    text-align: center;
       |    border: 1px solid rebeccapurple;
       |    border-radius: 5px;
       |    background-color: #0d2d48;
       |    background-image: linear-gradient(white 2px, transparent 2px), linear-gradient(90deg, white 2px, transparent 2px), linear-gradient(rgba(255,255,255,.3) 1px, transparent 1px), linear-gradient(90deg, rgba(255,255,255,.3) 1px, transparent 1px);
       |    background-size: 100px 100px, 100px 100px, 20px 20px, 20px 20px;
       |    background-position: -2px -2px, -2px -2px, -1px -1px, -1px -1px;
       |    color: white;
       |    box-shadow: 10px 5px 5px #4e4d4d;
       |""".stripMargin,
  " .cod-card.cod-card-10" -> "background-color: #99461b;",
  " .cod-card.cod-card-11" -> "background-color: #99461b;",
  " .cod-card.cod-card-12" -> "background-color: #99461b;",
  " .cod-card.cod-card-0" -> "background-color: #64000c;",
  " .cod-card:first-child" -> "grid-row: 1 / -1;"
).register()


case class CardsOfDoom(start:Int = 13, _myTurn:Boolean = true, gridLayout:Boolean = false) extends DHtmlComponent {

    val remaining = stateVariable(start)
    val myTurn = stateVariable(_myTurn)

    var iWillTake = choose()

    def choose():Int = {
      if (remaining.value > 0) {
        val mod = (remaining.value - 1) % 4
        val max = if (remaining.value > 3) 3 else remaining.value
        if (mod > 0) mod else Random.nextInt(max) + 1
      } else 0
    }

    def play(i:Int):Unit = {
      if (!myTurn.value && i > 0 && i < 4 && i <= remaining.value) {
        remaining.value = remaining.value - i
        myTurn.value = true
        iWillTake = choose()
      }
    }

    def playMyturn():Unit = {
      if (myTurn.value && remaining.value > 0) {
        remaining.value = remaining.value - iWillTake
        myTurn.value = false
      }
    }

    def iWin = remaining.value == 0 && myTurn.value

    def youWin = remaining.value == 0 && !myTurn.value

    def reset(won:Boolean) = {
      remaining.value = start
      myTurn.value = _myTurn
      iWillTake = choose()
    }

    override def render = {
        <.mutable.div(
          <.div(
            if (remaining.value > 0) {
              if (remaining.value > 1) {
                s"There are ${remaining.value} cards remaining."
              } else {
                "There is 1 card remaining."
              }
            } else {
              if (iWin) {
                <.div(
                  <.p("Oh no! You took the Card of Doom! I win this round!"),
                  <.button(^.cls := "btn btn-outline-primary", "Play again", ^.onClick --> reset(false))
                )
              } else {
                <.div(
                  <.p("Congratulations! I took the Card of Doom! You win!"),
                  <.button(^.cls := "btn btn-outline-primary", "Play again", ^.onClick --> reset(true))
                )
              }
            }
          ),
          <.div(^.cls := (if gridLayout then cardsOfDoomGridStyling else cardsOfDoomStyling),
            for (i <- 0 until remaining.value) yield {
              <.div(^.cls := s"cod-card cod-card-$i",
                i match {
                  case 0 => "A"
                  case 10 => "J"
                  case 11 => "Q"
                  case 12 => "K"
                  case _ => (i+1).toString
                } 
              )
            }
          ),
          <.div(
            if (remaining.value > 0) {
              if (myTurn.value) {
                <.div(
                  <.p(s"It's my turn. I will take ${ iWillTake } cards"),
                  <.button(^.cls := "btn btn-outline-primary", "Play my turn", ^.onClick --> playMyturn() )
                )
              } else {
                val max = if (remaining.value > 3) 3 else remaining.value

                <.div(
                  <.p("It's your turn. How many cards will you take?"),
                  for { i <- 1 to max } yield {
                    <.button(^.cls := "btn btn-outline-primary", s"Take $i", ^.onClick --> play(i))
                  }
                )
              }
            } else {
              <.div()
            }
          )
        )
    }

  

}
