package ai4p

import com.wbillingsley.veautiful.html.*
import com.wbillingsley.veautiful.doctacular.*
import org.scalajs.dom

/** The doctacular site */
val site = Site()

import typings.Marked

import com.wbillingsley.veautiful.doctacular.*
import Medium.* 

given markdown:Markup = Markup(Marked.parse(_))

@main def main() = {
    println("hello world")
    import site.given 

    parseLink(dom.window.location.hash)

    val n = dom.document.getElementById("render-here")
    n.innerHTML = ""

    Styles.installStyles()

    site.home = () => site.renderPage(Intro.frontPage)

    site.toc = site.Toc(
      "Home" -> site.HomeRoute,

      "State spaces" -> site.Toc(
        "Intro" -> site.addPage("statespaces", statespace.stateSpaceIntro),

        "Small games" -> site.add("smallGames",
          Alternative("Slide deck", Deck(() => statespace.smallgames)),
        ),

        "Heuristics" -> site.add("heuristics",
          Alternative("Slide deck", Deck(() => statespace.heuristics)),
        ),

        "Search strategies" -> site.add("searchStrategies",
          Alternative("Slide deck", Deck(() => statespace.searchStrategies)),
        ),
      ),

      "Learning by example" -> site.Toc(
        "Intro" -> site.addPage("fromexamples", fromexamples.fromExamplesIntro),

        "Salience" -> site.add("salience",
          Alternative("Slide deck", Deck(() => fromexamples.salience)),
        ),

        "Regression" -> site.add("regression",
          Alternative("Slide deck", Deck(() => fromexamples.regression)),
        ),

        "KMeans & KNN" -> site.add("kmeansknn",
          Alternative("Slide deck", Deck(() => fromexamples.kmeansknn)),
        ),

        "Decision Trees and Random Forests" -> site.add("decisions",
          Alternative("Slide deck", Deck(() => fromexamples.decisiontrees )),
        ),

      ),

      "Reasoning and Verification" -> site.Toc(
        "Intro" -> site.addPage("reasoning", reasoning.reasoningIntro),

        "Reasoning and Logic" -> site.add("reasoning",
          Alternative("Slide deck", Deck(() => reasoning.reasoningDeck )),
        ),

        "The Turing Test" -> site.add("turingtest",
          Alternative("Slide deck", Deck(() => reasoning.turingtest )),
        ),
      ),

      "Probability and Uncertainty" -> site.Toc(
        "Intro" -> site.addPage("uncertainty", uncertainty.uncertaintyIntro),

        "Monte Carlo Methods" -> site.add("montecarlo",
          Alternative("Slide deck", Deck(() => uncertainty.monteCarlo)),
        ),

        "Particle Filters" -> site.add("particleFilters",
          Alternative("Slide deck", Deck(() => uncertainty.particleFilters)),
        ),

        "Markov Models" -> site.add("markov",
          Alternative("Slide deck", Deck(() => uncertainty.markovModels)),
        ),

      ),

    )

    site.attachTo(n)
}