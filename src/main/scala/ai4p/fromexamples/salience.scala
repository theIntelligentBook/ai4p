package ai4p.fromexamples
import com.wbillingsley.veautiful.html.*
import com.wbillingsley.veautiful.doctacular.DeckBuilder

import <._
import ^._

import ai4p.{*, given}
import Common.*

import site.given
import scala.util.Random

type HappyGrid = ((Int, Int), (Int, Int))

extension (h:HappyGrid) {
    def happy:Boolean = 
        val ((a, b), (_, _)) = h
        b % 2 == 0
}

val happyStyle = new Styling(
    """|
       |border: 1px solid gray;
       |font-size: 25px;
       |display: inline-block;
       |margin: 1em;
       |font-family: "Fira Code";
       |
       |""".stripMargin
).modifiedBy(
    " td" -> "width: 75px; height: 50px; text-align: center; padding: 5px;",
    " .numbers" -> "font-size: 25px; font-weight: bold;",
    " .happy" -> "color: green;",
    " .unhappy" -> "color: red;",
)
.register()

case class HappyGame() extends DHtmlComponent {

    val maxNum = 10

    def newGrid = ((Random.nextInt(maxNum), Random.nextInt(maxNum)), (Random.nextInt(maxNum), Random.nextInt(maxNum)))
    
    val state = stateVariable(false)
    val grids = stateVariable(List(newGrid))

    def pushGrid():Unit = 
        grids.value = newGrid :: grids.value
        state.value = false

    def showHappiness():Unit =
        state.value = true

    def happyClass(show:Boolean, state:Boolean):String = 
        if show then 
            if state then "happy" else "unhappy"
        else "unknown"

    def renderGrid(h:HappyGrid, showHappy:Boolean = false) = {
        val ((a, b), (c, d)) = h

        <.div(^.cls := happyStyle,
            <.table(^.cls := happyClass(showHappy, h.happy),
                <.tr(^.cls := "numbers",
                    <.td(a.toString), <.td(b.toString)
                ),
                <.tr(^.cls := "numbers",
                    <.td(c.toString), <.td(d.toString)
                ),
                (if showHappy then 
                    <.tr(
                        <.td(^.cls := happyClass(showHappy, h.happy), ^.attr.colspan := "2", if h.happy then "Happy 😊" else "Sad 😞")
                    )
                else
                    <.tr(
                        <.td(^.cls := happyClass(showHappy, h.happy), ^.attr.colspan := "2", "?")
                    )
                )
            )
        ) 
    }

    override def render = {
        val head :: tail = grids.value : @unchecked // We know the list isn't empty from how it's initialised

        <.div(^.style := "overflow-y: auto; max-height: 900px;",
            renderGrid(head, state.value),

            (if state.value then 
                <.p(
                    <.button(^.cls := "btn btn-primary", "New grid", ^.onClick --> pushGrid())
                )
            else 
                <.p(
                    <.button(^.cls := "btn btn-primary", "Show happiness", ^.onClick --> showHappiness())
                )
            ),
        

            <.div(
                <.h3("Previously..."),
                for g <- tail yield renderGrid(g, true)
            )        
        )
       
    }


}


val salience = DeckBuilder(1920, 1080) 
  .markdownSlide(
    """
      |# Salience
      |
      |""".stripMargin
  ).withClass("center middle")
  .markdownSlides(
    """
    |## Human Learning
    |
    |Let's start with an outreach game that I use with visiting school-children.
    |
    |I'm going to show you some grids of numbers. Some of these are going to be "happy" grids and some will be "sad" grids.
    |
    |You're task is to figure out the rule for when a grid is happy.
    |
    |""".stripMargin)
  .veautifulSlide(<.div(
    <.h2("Human Learning"),
    <.p(HappyGame())
  ))
  .markdownSlides(
    """
    |## Classification
    |
    |Just in that game we've got a few things going on:
    |
    |* I was asking you to be a classifier. Classification tasks are very common for Machine Learning
    |
    |* Not all the information was relevant. As is the case for most machine learning, but it wasn't always obvious
    |  what was relevant or not.
    |
    |* Sometimes, we'll get a biased sample, for example far too many happy grids and not enough sad ones to figure out what's going on.
    |  
    |
    |---
    |
    |## Teachable Machine
    |
    |Now let's try one Google made earlier, that runs in a web-browser...
    |
    |[Teachable Machine, version 1](https://teachablemachine.withgoogle.com/v1/)
    |
    |If you click the link, you can try this at home as well. We're going to teach the comptuer to recognise a small number of items - in my case, road signs.
    |
    |
    |---
    |
    |## Confusion matrices
    |
    |Let's try out another version of the teachable machine. Again, you can do this at home, but what I want to show you is
    |in its advanced tab
    |
    |[Teachable Machine, image task](https://teachablemachine.withgoogle.com/train/image)
    |
    |If we have two classes that we're classifying items into, then if we take a sample where we know what the result should be, we can test the model against them.
    |That gives us a *confusion matrix* with four quadrants
    |
    |* True positives (or items in class A that were classified in class A)
    |* False positives (or items in class B that were classified in class A)
    |* False negatives (or items in class A that were classified in class B)
    |* True negatives (or items in class B that were classified in class B)
    |
    |""".stripMargin)
  .markdownSlide(willCcBy)
  .renderSlides
