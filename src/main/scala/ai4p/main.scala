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
      )

    )

    site.attachTo(n)
}