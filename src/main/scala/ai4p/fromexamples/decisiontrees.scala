package ai4p.fromexamples
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

import widgets.*


val decisiontrees = DeckBuilder(1920, 1080)
  .markdownSlide(
    """
      |# Decision Trees and Random Forests
      |
      |""".stripMargin
  ).withClass("center middle")
  .markdownSlides(
    """
    |## Decision Trees
    |
    |Way back in biology class in an English high school I was taught about classifying animals with a set of questions
    |
    |1. Does it have jointed legs, a hard outer shell (exoskeleton), and a body split into segments?
    |
    |  - Yes: It is an Arthropod.
    |
    |  - Sub-question: Does it have 8 legs? (Arachnid)
    |
    |  - Sub-question: Does it have 6 legs? (Insect)
    |
    |2. Does it have a soft, unsegmented body, sometimes covered by a hard shell (like a snail or squid)?
    |
    |  - Yes: It is a Mollusk.
    |
    |3. Does it have a soft, long, ringed, or segmented tube-like body (like an earthworm)?
    |
    |*Decision Trees* are somewhat like the machine learning equivalent
    |
    |---
    |
    |## Decision Trees
    |
    |Given some data, work out a set of questions about that data that will let us classify it. Now to find some questions ...
    |
    |We want questions that split our data into groups that are as pure as possible — mostly one class in each group.
    |
    |We measure "impurity" a little like a loss function. One of these is called *Gini impurity*. It's the probability that an
    |item, if we applied the model to it, would be mis-classified as belonging to a different group. 
    |
    |Gini = 1 − Σ (probability of class *c*)²
    |
    |* Gini = 0 means the group is perfectly pure (only one class present)
    |* Gini is higher the more mixed the classes are (i.e. the less the )
    |
    |---
    |
    |## Growing a tree
    |
    |Once we've picked the first question, we recursively keep examining whether splitting a group by asking *another* subquestion 
    |would improve the purity - this is called "Gini gain"
    |
    |* Keep splitting each group with its own best question
    |* Stop when a group is pure, too small to split further, or we hit a maximum depth
    |* Each "leaf" of the tree predicts whichever class is most common among the points that ended up there
    |
    |""".stripMargin)
  .veautifulSlide(<.div(
    <.h2("Growing a Decision Tree"),
    <.p(DecisionTreeWidget())
  ))
  .markdownSlides(
    """
    |## Overfitting
    |
    |If we let the tree grow very deep, it will happily carve out a tiny box just to capture a single stray point.
    |
    |* It gets 100% accuracy on the training data...
    |* ...but it's almost certainly *wrong* about the underlying pattern — it's memorised noise, not learned a rule
    |* This is called *overfitting*
    |
    |We usually control this with a *maximum depth*, a minimum number of points per leaf, or by *pruning*
    |branches back after growing the full tree.
    |
    |---
    |
    | ## The trouble with a single tree
    |
    |A decision tree is greedy — at every step it just picks whichever question looks best *right now*, with no
    |way to reconsider.
    |
    |* Small changes to the training data (a point moved, added, or removed) can change the first split entirely,
    |  and everything below it cascades
    |* A tree deep enough to fit the training data well is usually deep enough to have overfit it
    |
    |So a single tree tends to be *unstable* as well as prone to overfitting. 
    | 
    |
    |""".stripMargin)
    .imageSlide("Sir Francis Galton", "https://upload.wikimedia.org/wikipedia/commons/a/ae/Sir_Francis_Galton_by_Charles_Wellington_Furse.jpg")
    .markdownSlides(
    """
    |## The Wisdom of Crowds
    |
    |Sometimes there is an effect where a large number of *non-experts* can produce an accurate answer. 
    |
    |Essentially:
    |
    |* Every "guess" contains information and noise/error
    |* The errors are often random - so long as there's no reason for all the guessers to be biased in a particular direction
    |* If the errors are random (in every direction), then if you take the average of them, *the errors cancel each other out*
    |
    |"Random Forests" try to use this approach to improve Decision Trees.
    |
    |Rather than generate 1 tree, we'll generate several, each based on a random subset of the data.
    |
    |Then we let them vote, and hopefully their inaccuracies cancel each other out.
    |
    |---
    |
    |## Bootstrap aggregating ("bagging")
    |
    |The first source of randomness: give each tree a slightly different training set.
    |
    |* For each tree, draw a *bootstrap sample* — the same number of points as the original data, but sampled
    |  **with replacement** (so some points appear more than once, others not at all)
    |* Train a full tree on that sample
    |* Repeat for as many trees as you want in the forest
    |
    |Because each tree sees a different sample, they'll disagree — especially near the boundary, and especially
    |about outliers.
    |
    |---
    |
    |## Random feature selection
    |
    |The second source of randomness: at *each individual split*, only let the tree consider a random subset of
    |the features, rather than all of them.
    |
    |* This stops every tree converging on the same "obviously best" first question
    |* It decorrelates the trees further — even trees trained on similar data end up asking different questions
    |* Together with bagging, this is what turns "a bunch of decision trees" into a *random forest*
    |
    |With only two features (x and y, as below), this just means some trees are only allowed to split on x, and
    |others only on y — but even that's enough to make them meaningfully different from one another.
    |
    |""".stripMargin)
  .veautifulSlide(<.div(
    <.h2("One Tree vs a Forest"),
    <.p(RandomForestWidget())
  ))
  .markdownSlides(
    """
    |## Combining the votes
    |
    |Once every tree has been grown, a prediction is made by combining all of their answers:
    |
    |* **Classification** — each tree votes for a class, and the forest predicts whichever class got the most votes
    |* **Regression** — each tree predicts a number, and the forest predicts the average
    |
    |Individually, each tree is still a greedy, overfitted, somewhat unstable model. But their mistakes tend to be
    |in *different* places, so averaging them out cancels a lot of that noise — while the genuine pattern, which
    |every tree tends to pick up on, survives the averaging.
    |
    |---
    |
    |## Trade-offs
    |
    |* **Much more robust** — far less prone to overfitting than a single deep tree, and much less sensitive to
    |  small changes in the training data
    |* **Loses interpretability** — you can no longer point at "the" tree and read off the reasoning; a forest of
    |  100 trees doesn't reduce to one simple set of questions
    |* **More compute** — training (and running) many trees instead of one
    |* You can still ask "which features did the forest end up using the most, and how much did they help?" —
    |  this gives a *feature importance* ranking, a rough substitute for the interpretability a single tree gave us
    |
    |Random forests are a good example of a broader theme in machine learning: trading a little bit of
    |interpretability for a useful gain in robustness.
    |
    |""".stripMargin)

  .markdownSlide(willCcBy)
  .renderSlides
