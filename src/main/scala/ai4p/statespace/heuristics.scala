package ai4p.statespace
import com.wbillingsley.veautiful.html.{<, ^, unique}
import com.wbillingsley.veautiful.doctacular.DeckBuilder

import <._
import ^._

import ai4p.{*, given}
import Common.*

val heuristics = DeckBuilder(1280, 720) 
  .markdownSlide(
    """
      |# Heuristics
      |
      |
      |""".stripMargin
  ).withClass("center middle")
  .markdownSlides(
    """
    |## asdasd
    |
    |asdasd
    |
    |---
    |""".stripMargin
  )
  .markdownSlide(willCcBy)
  .renderSlides
