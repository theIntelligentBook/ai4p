package ai4p.uncertainty

import com.wbillingsley.veautiful.html.*

import ai4p.{*, given}
import Common._
import Styles._

import scala.scalajs.js

val uncertaintyIntro = <.div(
  chapterHeading(4, "Probability and Uncertainty", "images/uncertainty.jpg"),
  marked("""
    |In this topic we'll see
    |
    |* Markov Models, Hidden Markov Models, and Bayesian Networks
    |* Particle Filters
    |
    |""".stripMargin),
)
