package ai4p.reasoning.queens

import com.wbillingsley.veautiful.*
import com.wbillingsley.veautiful.html.*  
import com.wbillingsley.veautiful.doctacular.* 

import ai4p.{*, given}

val QueensStyle = Styling(
    """|
       |""".stripMargin
).modifiedBy(
     " .row" -> "display: flex;",
     " .cell" -> "display: inline-block; width: 30px; height: 30px; border: 1px solid #444; text-align: center; vertical-align: middle; line-height: 30px; padding-bottom: 3px;",
     " .cell.w" -> "background: #eee", " .cell.r" -> "background: #ff6d6d", " .cell.y" -> "background: #fdff52;",
     " .cell.g" -> "background:rgb(85, 172, 68);", " .cell.d" -> "background:rgb(144, 144, 144);",
     " .cell.c" -> "background:rgb(115, 209, 212);", " .cell.u" -> "background:rgb(161, 98, 202);",
     " .cell.p" -> "background:rgb(245, 139, 220);", " .cell.o" -> "background: #ffc067", " .cell.b" -> "background: #77f;",
     " .moveWidget" -> "fill: none; stroke: #0698b5; stroke-width: 4;",
     " .moveButton.black" -> "background: #444; color: white;",
     " .moveButton.white" -> "background: #eee; color: black;",
     " .queen" -> "font-weight: bold;",
     " .notqueen" -> "color: lightgrey;",
).register()

case class StudentException(callName:String, x:Throwable) extends RuntimeException(x) 

def studentCall[T](name:String, f: => T):T = try {
    f
} catch {
    case (x:Throwable) => throw StudentException(name, x)
}


class QueensSelector() extends DHtmlComponent with Keep("") {

    val state = stateVariable(puzzles.values.head)

    def render = {
        <.div(
            <.p(
                for (k, p) <- puzzles.toSeq yield 
                    <.button(k, ^.prop.disabled := (state.value == p), ^.onClick --> { state.value = p })
            ),
            QueensWidget(state.value)
        )
    }

}


class QueensWidget(grid:Grid) extends DHtmlComponent with Keep(grid) {

    val state = stateVariable(grid.allPossibilities)
    
    // Renders the state of the game
    def render =  try {
        <.div(^.cls := QueensStyle,

            (for (row, y) <- grid.zipWithIndex yield 
                <.div(^.cls := "row",
                    for (sq, x) <- row.zipWithIndex yield 
                        <.div(^.cls := "cell " + sq.letter, 
                            state.value((x, y)).toSeq match {
                                case Seq(Contents.Queen) => <.span(^.cls := "queen", "♕")
                                case Seq(Contents.Blank) => <.span(^.cls := "notqueen", "x")
                                case _ => " "
                            }
                        
                        )
                )
            ),

            <.button("Play AI move", ^.prop.disabled := state.value.solved, ^.onClick --> {  state.value = state.value.makeAstep(grid) }),

        )
    } catch {
        case (x:Throwable) => 
            // x.printStackTrace()
            <.div(
                <.h2("An exception occured"),
                <.pre(x.toString()),
                // <.pre(x.getStackTrace().mkString("\n")),
            )
    }

}


def queensPage = <.div(
    <.h2("LinkedIn Queens"),
    <.p("The widget below will let you pick a puzzle and progressively solve it with your solver."),
    QueensSelector()
)