package ai4p.neuralnets
import com.wbillingsley.veautiful.html.*
import com.wbillingsley.veautiful.doctacular.DeckBuilder

import <._
import ^._

import ai4p.{*, given}
import Common.*

import site.given

import ai4p.neuralnets.widgets.*


val neuralnets = DeckBuilder(1920, 1080)
  .markdownSlide(
    """
      |# Neural networks
      |
      |""".stripMargin
  ).withClass("center middle")
  .markdownSlides(
    """
      |## Scalable approximations
      |
      |Often, in different fields, we'll come up with a way that we can approximate something
      |using a simple mechanism where when we add more of it, we get a finer level of detail
      |
      |
      |
      |""".stripMargin
  )
  .veautifulSlide(<.div(
    <.h2("Adding frequencies"),
    markdown.div(
      """
        |In engineering, we often use "Fourier transforms"
        |
        |These essentially say that a function can be expressed as a a sum of sine waves
        |at different frequencies. 
        |
        |Musicians might see something similar, as "low pass filters" take out the high
        |frequencies but leave the low frequenc
        |""".stripMargin
    ),
    <.p(HarmonicsWidget())
  ))
  .veautifulSlide(<.div(
    <.h2("Adding powers of x"),
    markdown.div(
      """|Taylor series are useful in engineering mathematics, because they let any function
         |be expressed (theoretically) as a "polynomial" - different weights on different powers of x 
         |
        |""".stripMargin
    ),
    <.p(TaylorSeriesWidget())
  ))
  .veautifulSlide(<.div(
    <.h2("The trapezium rule & Riemann sums"),
    markdown.div(
      """
        |Back in school, yiou might remember the "trapezium rule", where you estimate the area under
        |a curve by drawing trapeziums under it and adding up their areas. More trapeziums = more accuracy.
        |
        |As a child, I always thought "why bother with the trapezium" - rectangles are easier to work out
        |and at the "limit" (as the width goes to zero) they also converge on the area under the curve.
        |
        |The version with rectangles happens to be called a Riemann sum.
        |""".stripMargin
    ),
    <.p(RiemannWidget())
  ))
  .markdownSlides(
    """
      |## Universal approximation theorem
      |
      |The universal approximation theorem is AI's version of this.
      |
      |Let's take 2 "step functions". One up, one down.
      |
      |// Insert something here that shows these composing into a step.
      |
      |We've now got something that can produce "rectangles under the curve", so 
      |if we add enough of them, we can approximate any function in x.
      |
      |""".stripMargin
  )
  .veautifulSlide(<.div(
    <.h2("Pulses, and a network built from them"),
    markdown.div(
      """Each pulse below is made from **two** step-activated hidden neurons (one switching on, one
        |switching off). Growing the slider grows the hidden layer - watch the network diagram widen
        |as the approximation improves.
        |
        |Note that we're *not doing any training here*. We're just showing how simple nodes using step functions can
        |give us approximately the right output if we know what weights to give them.
        |""".stripMargin
    ),
    <.p(PulseNetworkWidget())
  ))
  .markdownSlides(
    """
      |## Depth works too
      |
      |We can also get more accuracy from more elements if we make them deeper, too.
      |(Though it's not quite so simple to show.)
      |
      |""".stripMargin
  )
  .veautifulSlide(<.div(
    <.h2("Two neurons wide, getting deeper"),
    markdown.div(
      """
        |Theoretically, a network that can be infinitely *deep* can apprximate any function so long
        |as its hidden layers are wider than the input. And it's more efficient, as each successive
        |layer refines the last layer, rather than just being a new patch.
        |
        |In practice, it's hard to work out the weights for a very deep network though so, let's just
        |do it for a quadratic function that's easy to estimate to show the principle.
        |
        |Each node has a "v" shaped function (also called "tent"), and as we combine more and more of them, we get closer and
        |closer to the curve.
        |""".stripMargin
    ),
    <.p(DeepNarrowWidget())
  ))
  .veautifulSlide(<.div(
    <.h2("Wider (but still narrow), and trained"),
    markdown.div(
      """
        |Those networks weren't trained. They were simple enough just to *calculate* the weights.
        |
        |This one is trained, but it should be simple enough to see how adding more neurons lets us
        |get more detail into how it learns the function.
        |""".stripMargin
    ),
    <.p(WideDeepWidget())
  ))
  .markdownSlides(
    """
      |## Not just weights. Activation functions too. 
      |
      |Neural networks don't just have a set of weights. They also have "activation functions"
      |that determine how those weights will shape the output. 
      |
      |""".stripMargin
  )
  .veautifulSlide(<.div(
    <.h2("Without activation functions"),
    markdown.div(
      """
        |If our network just had a set of weights, each neuron would be linear in its output.
        |
        |When you stack them, they'd still be linear.
        |
        |""".stripMargin
    ),
    <.p(NeuronChainWidget(Vector("linear" -> NeuronChainWidget.linear), yMin = -10, yMax = 10, showBias = true))
  ))
  .veautifulSlide(<.div(
    <.h2("Step and sigmoid activation functions"),
    markdown.div(
      """
        |In our pulse example earlier on, we used two step nodes to make a pulse. But they were
        |softened into "sigmoids".
        |
        |Steps can work to *represent* any function, but the problem is in the training.
        |
        |Training a neural network uses "gradient descent", propagating errors back through the network
        |and using the gradient of the activation function to know how much to adjust a weight or bias by.
        |
        |Step functions have no useful gradient. They are either flat or vertical. So we need the "sigmoid"
        |softened step for gradient descent to work.
        |
        |""".stripMargin
    ),
    <.p(NeuronChainWidget(
      Vector("step" -> NeuronChainWidget.step, "sigmoid" -> NeuronChainWidget.sigmoid),
      reps = 1, expandLinear = true,
      yMin = -0.2, yMax = 1.2, wMin = -3.0, wMax = 3.0, showBias = true, bMin = -3.0, bMax = 3.0
    ))
  ))
  .veautifulSlide(<.div(
    <.h2("ReLU: cheap, and used in teaching courses"),
    markdown.div(
      """**ReLU** (Rectified linear unit) just clips negative values to zero and leaves positive
        |values untouched.
        |
        |It's a very simple nonlinearity (one corner) and cheap to compute, so is often used in neural network teachng courses.
        |
        |But try making one weight negative: once a ReLU neuron's output is clipped to zero, multiplying it
        |by a negative weight and clipping again just gives zero again - this route through the network has "died" for that input.
        |
        |Although often one of the first activation functions taught, ReLU doesn't perform as well in deep networks as smoother functions
        |""".stripMargin
    ),
    <.p(NeuronChainWidget(Vector("ReLU" -> NeuronChainWidget.relu), reps = 2, expandLinear = true, showBias = true, yMin = -2, yMax = 10))
  ))
  .veautifulSlide(<.div(
    <.h2("SiLU, also called 'swish'"),
    markdown.div(
      """
        |SiLU ("sigmoid linear unit") is the activation function used in most modern large language models. 
        |
        |The activation here is **SiLU** (`z * sigmoid(z)`), also called Swish) 
        |
        |It looks a bit like ReLU, but it's smoother and has a little dip before the bend.
        |""".stripMargin
    ),
    <.p(NeuronChainWidget(
      Vector("SiLU" -> NeuronChainWidget.silu),
      reps = 1, expandLinear = true, showBias = true,
      xMin = -5, xMax = 5, yMin = -1.5, yMax = 10, wMin = -2.5, wMax = 2.5, bMin = -2.5, bMax = 2.5
    ))
  ))
  .veautifulSlide(<.div(
    <.h2("SwiGLU: gating two branches together"),
    markdown.div(
      """
        |This tries to model what goes on in most LLM neurons.
        |SwiGLU is "Swish Gated Linear Unit"
        |
        |There's two paths through the neuron:
        |  - one goes through a Swish function
        |  - the other is just linear  
        |
        |And then you multiply them together
        |
        |We've also dropped the "bias" parameters, because most LLMs omit them. 
        |""".stripMargin
    ),
    <.p(SwiGLUWidget())
  ))
  .markdownSlides(
    """
      |## A small network
      |
      |Let's put width, depth, and learning together in one small, fully worked example: a network
      |that *learns* to recognise a handful of pixelated digits, with backpropagation happening live,
      |right in front of you.
      |
      |""".stripMargin
  )
  .veautifulSlide(<.div(
    <.h2("A tiny feed-forward neural network in action"),
    <.p(TinyDigitNetWidget())
  ))
  .markdownSlides(
    """
      |## Relationships between relationships
      |
      |That network could only ever recognise *exactly* the pixel patterns it was shown. Nudge a
      |digit one pixel over, and every one of its 25 inputs changes at once - as far as a single
      |hidden layer is concerned, it may as well be a completely different picture.
      |
      |Adding a **second** hidden layer changes what the network is able to represent:
      |
      |* The **first** hidden layer can learn small, local patterns - roughly "is there a stroke
      |  around about here"
      |* The **second** hidden layer combines those into relationships *between* those patterns -
      |  "this arrangement of strokes, wherever exactly they sit, is a loop"
      |
      |""".stripMargin
  )
  .veautifulSlide(<.div(
    <.h2("An example with two hidden layers"),
    <.p(DeepDigitNetWidget())
  ))
  .markdownSlides(
    """
      |## Here's one (someone else) made earlier
      |
      |https://cs.stanford.edu/people/karpathy/convnetjs/
      |
      |""".stripMargin
  )
  .veautifulSlide(<.div(
    <.h2("Convolutional layers"),
    markdown.div(
      """
        |Neural networks for image recognition will often use "convolutional" layers.
        |
        |This learns a "kernel" to apply on a sliding window across the image to detect low-level features.
        |
        |This one's been hard-coded to detect vertical edges, but a trained one in a neural network could "learn" to detect other interesting features.
        |""".stripMargin
    ),
    <.p(ConvKernelWidget())
  ))
  .veautifulSlide(<.div(
    <.h2("Recurrent neural networks"),
    markdown.div(
      """
        |Sometimes, we have values that happen over time
        |
        |* words in a sentence
        |* weather data
        |
        |Recurrent neural networks add loops into the network (layers can feed back on themselves) to try to learn how to capture state as it evolves...
        |
        |... but LLMs don't do this!
        |""".stripMargin
    ),
  ))
  .markdownSlides("""|## Learning to play Atari games. (DeepMind, 2013)
                     |
                     |<img src="images/spaceinvaders.jpg" width="480" />
                     |<img src="images/demonattack.jpg" width="480" />
                     |
                     |Q learning:
                     |
                     |Learn to predict the "quality" of an action
                     |
                     |Input is the screen of the game, plus an action (e.g. move left)
                     |
                     |Output is what that action is "worth" (will the game score go up?)
                     |
                     |---
                     |
                     |## Learning more than one game at once (Mark Mackenzie, 2017)
                     |
                     |<img src="images/tsneplot.jpg" width="480" />
                     |
                     |Make it play two games simultaneously
                     |
                     |Picture shows a t-SNE plot, trying to look into how the neural network divided up its state space for the games
                     |
                     |""".stripMargin)
  .markdownSlide(willCcBy)
  .renderSlides
