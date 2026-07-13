package ai4p.fromexamples

import com.wbillingsley.veautiful.html.*

import ai4p.{*, given}
import Common._
import Styles._

import scala.scalajs.js

val fromExamplesIntro = <.div(
  chapterHeading(2, "Learning by Example", "images/simongame.jpg"),
  marked("""
    |Sometimes, we'll want the computer to learn from things it has seen. i.e. In this topic we'll talk a little bit about machine learning.
    |
    |In this topic we'll see
    |* A general introduction to machine learning and the idea of [salience](#/decks/salience/0) - what information turns out to be important and how can we know?
    |* [Regression](#/decks/regression/0) and trying to determine how much impact something has on some data
    |* Grouping things together and clustering
    |
    |We'll also have a philosophical aside
    |* Another way of looking at bias 
    |
    |""".stripMargin),
)
