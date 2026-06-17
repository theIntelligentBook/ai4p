package ai4p

import com.wbillingsley.veautiful.html.{<, ^, unique}
import site._

object Intro {

  val frontPage = <.div(
    <.div(^.attr("style") := "position: relative; top: 0;",
      <.img(^.src := "images/ai4p banner.jpg", ^.alt := "Thinking About Programming", ^.style := "max-width: 100%;"),
      <.div(^.attr("style") := "position: absolute; bottom: 0px; width: 100%; text-align: center; ",
        markdown.div("# *AI4P. A little OER on how machines think*")
      )
    ),
    <.div(
      <.div(^.cls := "lead",
        <.dynamic.p("Welcome", serverLink.dynamic.map {
          case Some(sl) => 
            sl.name.getOrElse(" Unknown Student")
          case _ => "Unknown Student"
        }),
        markdown.div(
          """
            | AI is everywhere, but not everyone is a coder or a mathematician. 
            | If you're going to work with it, though, it's probably important to have a mental model of how it works.
            | This little OER tries to give you some intuition on what's going on inside various kinds of artificial intelligence.
            |
            | This site works a little like an open source textbook. It's an introduction to programming for adults
            | and children, that talks as much about "how to think about programming" as about syntax. I use JavaScript
            | as the language, so you can try programming things right in the site itself, but there's also a blocks
            | programming environment, robot mazes, and various other things to try to help you along.
            |""".stripMargin
        )
      ),
      markdown.div(
        s"""
           |""".stripMargin
      )
    ),
    Seq.empty
  )

}
