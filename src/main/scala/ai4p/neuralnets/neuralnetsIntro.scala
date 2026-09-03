package ai4p.neuralnets

import com.wbillingsley.veautiful.html.*

import ai4p.{*, given}
import Common._
import Styles._

import scala.scalajs.js

val neuralnetsIntro = <.div(
  chapterHeading(6, "Neural Networks", "images/uncertainty.jpg"),
  marked("""
    |Neural networks are inspired by how human brains work - but they aren't quite like human brains
    |
    |In this topic we'll see
    |
    |* Neural Networks, and 
    |* Embeddings and how words (and tokens) can be represented as a point in a (higher dimensional) space
    |* Attention and how it's used by Large Language Models (LLMs) to work efficiently over very large amounts of language data
    |
    |We'll also have a short aside on creativity
    |
    |""".stripMargin),
)
