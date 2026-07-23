package ai4p.reasoning

import com.wbillingsley.veautiful.html.*

import ai4p.{*, given}
import Common._
import Styles._

import scala.scalajs.js

val reasoningIntro = <.div(
  chapterHeading(3, "Reasoning and Verification", "images/proofpic.jpg"),
  marked("""
    |Sometimes, we'll use a more bespoke chain of reasoning, to work out whether something is definitely true.
    |
    |As logic and reasoning are also often a part of philosophy, we'll also use this topic to discuss the question of
    |consciousness and AI.
    |
    |""".stripMargin),
)
