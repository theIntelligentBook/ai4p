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
      |Alan Turing, 1950. Computing Machinery and Intelligence, *Mind* 49:433-460
      | 
      |""".stripMargin)
  .imageSlide("The imitation game", "images/imitationgame.jpg")
  .imageSlide("The Turing Test", "images/turingtest.jpg")
  .markdownSlides(
    """
      |
      |> I believe that in about fifty years' time it will be possible, to programme computers ... to
      |> make them play the imitation game so well that an average interrogator will not have
      |> more than 70 per cent chance of making the right identification after five minutes of
      |> questioning. The original question, "Can machines think?" I believe to be too
      |> meaningless to deserve discussion. Nevertheless I believe that at the end of the century
      |> the use of words and general educated opinion will have altered so much that one will be
      |> able to speak of machines thinking without expecting to be contradicted. 
      |
      |Technically, not a bad prediction, but... 
      | 
      |""".stripMargin)
  .imageSlide("ELIZA", "https://upload.wikimedia.org/wikipedia/commons/7/79/ELIZA_conversation.png")
  .markdownSlides(
    """
      |### We're not very good at this test...
      |
      |* Phishing
      |
      |* Catfish
      |
      |Humans can be fooled by someone determined to fool them
      |
      |If I can convince you I'm from Yorkshire, does that make me really from Yorkshire?
      |
      |---
      |
      |### The Argument from Consciousness
      |
      |> Not until a machine can write a sonnet or compose a concerto
      |> because of thoughts and emotions felt, and not by the chance fall of symbols, could we
      |> agree that machine equals brain-that is, not only write it but know that it had written it.
      |> No mechanism could feel (and not merely artificially signal, an easy contrivance)
      |> pleasure at its successes, grief when its valves fuse, be warmed by flattery, be made
      |> miserable by its mistakes, be charmed by sex, be angry or depressed when it cannot get
      |> what it wants
      |
      |Jeffeson Lister, 1949
      |
      |Turing: "According to the most extreme form of this view the only way by which one could 
      |be sure that machine thinks is to be the machine and to feel oneself thinking. ... It is in fact the solipsist point of view."
      | 
      |""".stripMargin)
  .imageSlide("The Rocky Horror Show, ADMS 2026. Photo Oz Eye View", "images/rockyhorrornarrator.jpeg")
  .markdownSlides(
    """| ### What is it like to be a bat?
      |
      |1974 Philosophy paper by Thomas Nagel
      |
      |> ... fundamentally an organism has conscious mental states if and
      |> only if there is something that it is like to be that organism-
      |> something it is like for the organism.
      |> We may call this the subjective character of experience. It is
      |> not captured by any of the familiar, recently devised reductive
      |> analyses of the mental, for all of them are logically compatible
      |> with its absence. It is not analyzable in terms of any explanatory
      |> system of functional states, or intentional states, since these could
      |> be ascribed to robots or automata that behaved like people though
      |> they experienced nothing.
      |
      |---
      |
      |### The inverted spectrum
      |
      |<div style="height: 200px; width: 200px; background: red;"></div>
      |
      |John Locke, 17th century
      |
      |---
      |
      |### The inverted spectrum
      |
      |<div style="height: 200px; width: 200px; background: green;"></div>
      | 
      |John Locke, 17th century
      |
      |---
      |
      |### Mental imagery...
      |
      |Close your eyes. Now picture an apple.
      |
      |---
      |
      |### Mental imagery...
      |
      |What colour was the apple?
      |
      |- Red
      |- Green
      |- It didn't have a colour until I asked you what colour it was
      |- It did have a colour, you just couldn't see it
      |
      |Aphantasia - differences in how we generate mental imagery
      |
      |""".stripMargin)
    .imageSlide("An image not quite how I 'pictured' it even though I couldn't", "images/gpt from the wings.jpg")
    .markdownSlides(
    """|
      |### For AI, we are inevitably reductionist
      |
      |To get an AI to produce an image, we have to *give it* functionality to produce images
      |
      |In which case, of course it's "experience" of an image is physical
      |
      |
      |""".stripMargin)
    .imageSlide("Random noise, perturbed to maximise an AI classifier", "https://2.bp.blogspot.com/-17ajatawCW4/VYITTA1NkDI/AAAAAAAAAlM/eZmy5_Uu9TQ/s1600/classvis.png")
    .imageSlide("An LLM's concept of the most banana-like banana", "//turing.une.edu.au/~wbilling/faroutscience/davinci2 banana.jpg")
    .markdownSlides(
    """
      |### Another problem...
      |
      |How much of you is really you?
      |
      |
      |""".stripMargin)
    .imageSlide("Noises Off, ADMS 2025", "images/noises_off_act3.jpeg")
    .markdownSlides(
    """
      |### Does reality show up the flaws in philosophy?
      |
      |* From your own experience, you can't prove the world is real (you can't disprove solipsism)
      |
      |* From external obersvation, someone else can't prove your internal experience (they can't disprove reductionism)
      |
      |That seems to be inherent to our existence and it feels like an interesting design choice for existence -- most mathematical universes aren't like that. 
      |(There is no "what it's like to be a chess pawn", most games we invent aren't played from inside them.)
      |
      |In the 1950s "What if an AI could mimic us enough that we couldn't tell the difference" seemed like a remote thought experiment.
      |This may have made it easier to romanticise the idea of AI as thought or experience, and notions like the Turing test.
      |
      |Now, we're dealing with the mucky problem of telling AI work from human work every day. Now we've experiened it, though, we tend to think of AI mimicry as fakery -- deepfakes. 
      |The proximity has exposed that just because an AI can mimic us *doesn't* mean we have to assume it's like us.
      |
      |We *can* be slightly solipsist and assume that the universe exists for human experience, because regardless of how well something can mimic our 
      |behaviour, our own is the only experience we definitely know exists; the rest is just assumptions we can choose to make or not.
      |
      |Futurists have often fancifully written as if AI is the search for artificial *consciousness*, but it's probably better to think of it as artificial subconsciousness
      |
      |""".stripMargin
    )
  .markdownSlide(willCcBy)
  .renderSlides
