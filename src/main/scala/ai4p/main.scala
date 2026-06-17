package ai4p

import com.wbillingsley.veautiful.html.*
import com.wbillingsley.veautiful.doctacular.*
import org.scalajs.dom

/** The doctacular site */
val site = Site()

import typings.marked.mod.marked
given markdown:Markup = Markup(marked(_))

@main def main() = {
    println("hello world")

    parseLink(dom.window.location.hash)

    val n = dom.document.getElementById("render-here")
    n.innerHTML = ""

    Styles.installStyles()

    site.home = () => site.renderPage(Intro.frontPage)
    site.attachTo(n)
}