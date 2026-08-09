package ai4p.uncertainty
import com.wbillingsley.veautiful.html.*
import com.wbillingsley.veautiful.doctacular.DeckBuilder

import <._
import ^._

import ai4p.{*, given}
import Common.*
import ai4p.uncertainty.widgets.{BridgeOpenWidget, BridgeHiddenWidget, ParticleFilter1D, ParticleFilter2D, Suit, Seat}


import canvasland.{CanvasLand, LineTurtle, Turtle}
import coderunner.JSCodable


import site.given

val scilly = <.img(^.src := "assets/images/scilly isles.jpg").build().create()

val particleFilters = DeckBuilder(1920, 1080)
  .markdownSlide(
    """
      |# Particle Filters
      |
      |""".stripMargin
  ).withClass("center middle")
  .imageSlide("Engraving of the Scilly Naval Disaster 1707", "https://upload.wikimedia.org/wikipedia/commons/d/d5/HMS_Association_%281697%29.jpg")
  .veautifulSlide(<.div(
      <.h2("Sailing into the Channel"),
      markdown.div(
        """We should be able to sail into the Channel with
          |
          |```js
          |left(30); forward(600); right(20); forward(300);
          |```
          |
          |But what if storms, winds, or currents means our first leg was a little bit wrong?
          |""".stripMargin),
      JSCodable(CanvasLand()(
        viewSize = 920 -> 640,
        fieldSize = 1280 -> 720,        
        r = Turtle(50, 600),
        setup = (c) => {
          c.drawImage(scilly, 0, 0,1280, 720, 0,0,1280, 720)
        }
      ))(tilesMode = false, fontSize = 20)
    ))
  .markdownSlides(
    """
      |
      |## Robot vacuum cleaners
      |
      |Let's think about how a robot vacuum cleaner can find its way around your house
      |
      |For the moment, assume the house is already mapped. 
      |
      |It has a distance sensor that can see how far it is from objects, but it's a bit noisy. 
      |And GPS doesn't work very well indoors, so all it's got to go on is its distance sensor and 
      |"dead reckoning"
      |
      |So, we don't have *any* certain information at all:
      |
      |* Our sensors might not be quite right
      |* If we move, our new position depends on where we were (which we didn't know) and our
      |  wheels might have slipped on the carpet and we mightn't be quite where we thought.
      |
      |---
      |
      |## The particle filter idea
      |
      |We're going to generate a lot of random positions for our robot, and then we're going to
      |
      |* Rank them based on how well they match what we know (the sensor data)
      |* Generate a new population of random positions by sampling from the pool, 
      |  but more often picking the ones that seemed more "right"
      |* Sense again and keep going.
      |
      |i.e. we're going to do some "survival of the fittest" so our crowd of possible locations
      |gets better over time.
      |
      |Hopefully after a few cycles:
      |
      |* Each **particle** is a *guess* at the true state (e.g. a candidate `(x, y)` position)
      |* Where there are lots of particles, the robot is more likely to be
      |* Where there are few or none, it's unlikely to be
      |
      |More particles agreeing on roughly the same place is what "confident belief" looks like. A wide
      |scatter of particles is what "I genuinely don't know" looks like — including believing several
      |*different* places at once, exactly like the two-humped belief you may have seen in the bridge
      |bot's card-location bars.
      |
      |""".stripMargin
  )
  .markdownSlides(
    """
      |## The algorithm
      |
      |Every time-step, a particle filter repeats a loop of four operations:
      |
      |1. **Predict** — move every particle according to the motion model (e.g. "drove forward 2m"), each with a little independent noise added
      |2. **Update** — take a sensor reading, and *weight* each particle by how well its position would explain that reading
      |3. **Resample** — draw a new set of particles, picking each old particle with probability proportional to its weight
      |4. Go back to step 1
      |""".stripMargin
  )
  .veautifulSlide(<.div(
    <.h2("A one-dimensional simplification"),
    markdown.div(
      """|The common example for demonstrating the idea has an imaginary robot in one dimension (like a hallway)
         |and three doors. 
         |
         |The only information it has is the (noisy) sensor readings of how far it is from a door
         |
         |""".stripMargin
    ),
    <.p(ParticleFilter1D())
  ))
  .markdownSlides(
    """
      |## Predict: particles spread out; Resample: they cluster
      |
      |Because every particle gets its own little bit of random noise on top of the same commanded motion, the particle
      |cloud tends to *spread out* a bit after every Predict step.
      |
      |That's honest: the further the robot moves without checking its sensors, the less certain we should be about exactly
      |where it ended up.
      |
      |**Resampling** then throws away the improbable guesses and duplicates the good ones — so, over time, particles pile
      |up around wherever keeps explaining the sensor readings well.
      |
      |---
      |
      |## Why resampling matters: degeneracy
      |
      |Watch the **ESS** (Effective Sample Size) badge on the widget. It's computed as
      |
      |`ESS = 1 / Σ(weight²)`
      |
      |right after weighting, *before* resampling happens.
      |
      |* If weights are roughly equal, ESS is close to the full particle count `n` — every particle is still "pulling its weight"
      |* If one or two particles have grabbed almost all the weight, ESS collapses towards 1 — most particles are now
      |  wasted, sitting somewhere the evidence says is very unlikely
      |
      |This is **particle degeneracy**, and it's the reason we resample every round rather than just re-weighting forever:
      |resampling turns "a few particles with huge weights" back into "many equally-weighted particles in the right place".
      |
      |
      |""".stripMargin
  )
  .veautifulSlide(<.div(
    <.h2("Now in 2D"),
    markdown.div(
      """|The hallway example is easier to visualise, but the same loop works for any state space — including a robot's
         |`(x, y, heading)` pose in a room.
         |
         |Instead of one "nearest door" reading, this robot senses its range to *every* item it can "see" at once. 
         |
         |""".stripMargin
    ),
    <.p(ParticleFilter2D())
  ))
  .markdownSlides(
    """
      |## Strengths and weaknesses
      |
      |**Strengths**
      |
      |* Simple to implement: it's just "simulate, weight, resample"
      |* Doesn't make any complex assumptions about the situation (e.g. that movement is linear) that exact mathematical estimation methods would need.
      |
      |**Weaknesses**
      |
      |* You need *enough* particles to cover the state space. For 2D that's fine. For higher dimensional data it can become impractical.
      |* It's non-deterministic. Run it twice, get slightly different answers
      |* Resampling too often (or too aggressively) can cause **particle impoverishment** — collapsing to only a handful of
      |  distinct positions, and losing the ability to recover if that turns out to be wrong
      |
      |""".stripMargin
  )
  .markdownSlide(willCcBy)
  .renderSlides 
