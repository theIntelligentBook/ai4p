package ai4p

import com.wbillingsley.veautiful.html.*
import com.wbillingsley.veautiful.doctacular.*
import org.scalajs.dom

/** The doctacular site */
val site = Site()

import typings.marked.mod.marked
import com.wbillingsley.veautiful.doctacular.*
import Medium.* 

given markdown:Markup = Markup(marked(_))

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

      ),

    )

    site.attachTo(n)
}