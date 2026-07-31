package ai4p.reasoning
import com.wbillingsley.veautiful.html.*
import com.wbillingsley.veautiful.doctacular.DeckBuilder

import <._
import ^._

import ai4p.{*, given}
import Common.*

import site.given
import scala.util.Random

import site.given
import scala.util.Random

import org.scalajs.dom


val turingtest = DeckBuilder(1920, 1080)
  .markdownSlide(
    """
      |# The Turing Test and all that...
      |
      |""".stripMargin
  ).withClass("center middle")
  .markdownSlides(
    """
      |
      |> I propose to consider the question, "Can machines think?"
      |
      |...upload the rest tomorrow
      | 
      |""".stripMargin)
  .imageSlide("The imitation game", "images/imitationgame.jpg")
  .imageSlide("The Turing Test", "images/turingtest.jpg")
  .markdownSlide(willCcBy)
  .renderSlides
