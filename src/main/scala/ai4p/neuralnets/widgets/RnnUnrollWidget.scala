package ai4p.neuralnets.widgets

import com.wbillingsley.veautiful.html.{<, Styling, DHtmlComponent, VHtmlContent, ^}
import scala.collection.mutable.ArrayBuffer

import ai4p.{*, given}

/**
 * A simple, mechanism-only (not trained) illustration of a recurrent network: a short input
 * sequence is consumed one step at a time, and each step's hidden state is computed from *both*
 * the new input and the previous hidden state - so as you reveal steps left to right, you're
 * watching the network's "memory" of everything it's seen so far get folded into three numbers.
 */
object RnnUnrollWidget {
  val hiddenSize = 3

  /** A short toy input sequence to consume one step at a time. */
  val sequence: Vector[Double] = Vector(1.0, -1.0, 1.0, 1.0, -1.0)

  /** Fixed (hand-chosen, not learned) recurrence weights: h_t = tanh(Wx*x_t + Wh . h_{t-1} + b) */
  val wx: Vector[Double] = Vector(0.9, -0.6, 0.4)
  val wh: Vector[Vector[Double]] = Vector(
    Vector(0.5, -0.2, 0.1),
    Vector(0.1, 0.4, -0.3),
    Vector(-0.2, 0.2, 0.6)
  )
  val bias: Vector[Double] = Vector(0.0, 0.0, 0.0)

  def step(x: Double, hPrev: Vector[Double]): Vector[Double] =
    Vector.tabulate(hiddenSize) { j =>
      val recurrent = (0 until hiddenSize).map(i => wh(j)(i) * hPrev(i)).sum
      math.tanh(wx(j) * x + recurrent + bias(j))
    }

  val styling = Styling(
    """|display: inline-block;
       |font-family: 'Lato', sans-serif;
       |""".stripMargin
  ).modifiedBy(
    " .rn-row" -> "display: flex; align-items: center; gap: 6px; flex-wrap: wrap; margin: 10px 0;",
    " .rn-step" -> "display: flex; flex-direction: column; align-items: center; gap: 4px; border: 1px solid #e2e8f0; border-radius: 6px; padding: 6px 8px;",
    " .rn-arrow" -> "font-size: 1.3rem; color: #94a3b8;",
    " .rn-x" -> "font-family: monospace; font-size: 0.85rem; padding: 1px 6px; border-radius: 3px; background: #f1f5f9;",
    " .rn-h-label" -> "font-size: 0.7rem; color: #666;",
    " .rn-h" -> "display: flex; gap: 2px;",
    " .rn-cell" -> "width: 16px; height: 16px; border-radius: 3px; border: 1px solid #94a3b8;",
    " .rn-controls" -> "margin-top: 8px; display: flex; gap: 8px; align-items: center;",
    " .rn-info" -> "font-family: monospace; font-size: 0.85rem; color: #444; margin-top: 6px;"
  ).register()
}

case class RnnUnrollWidget() extends DHtmlComponent {
  import RnnUnrollWidget._

  private val history: ArrayBuffer[Vector[Double]] = ArrayBuffer(Vector.fill(hiddenSize)(0.0))

  private def advance(): Unit =
    if history.size - 1 < sequence.size then
      history += step(sequence(history.size - 1), history.last)
      rerender()

  private def reset(): Unit =
    history.clear()
    history += Vector.fill(hiddenSize)(0.0)
    rerender()

  private def hiddenBadge(h: Vector[Double], label: String): VHtmlContent =
    <.div(^.cls := "rn-step",
      <.div(^.cls := "rn-h-label", label),
      <.div(^.cls := "rn-h",
        for v <- h yield <.div(^.cls := "rn-cell", ^.style := s"background: ${NetworkView.divergingColor(v)};")
      )
    )

  override protected def render =
    val consumed = history.size - 1

    <.div(^.cls := styling.className,
      <.div(^.cls := "rn-row",
        hiddenBadge(history.head, "h0 (start)"),

        for t <- 1 to consumed yield
          <.div(^.cls := "rn-row",
            <.span(^.cls := "rn-arrow", "→"),
            <.div(^.cls := "rn-step",
              <.div(^.cls := "rn-h-label", s"x${t}"),
              <.div(^.cls := "rn-x", f"${sequence(t - 1)}%+.0f")
            ),
            <.span(^.cls := "rn-arrow", "→"),
            hiddenBadge(history(t), s"h$t")
          )
      ),

      <.div(^.cls := "rn-info",
        if consumed < sequence.size then s"consumed $consumed / ${sequence.size} inputs - each h(t) depends on x(t) AND h(t-1)"
        else "full sequence consumed - h(t) now reflects the whole input history, folded down to 3 numbers"
      ),

      <.div(^.cls := "rn-controls",
        <.button(^.cls := "btn btn-outline-primary btn-sm",
          ^.onClick --> advance(), if consumed >= sequence.size then "Done" else "Step"),
        <.button(^.cls := "btn btn-outline-secondary btn-sm", ^.onClick --> reset(), "Reset")
      )
    )
}
