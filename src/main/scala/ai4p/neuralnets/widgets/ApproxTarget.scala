package ai4p.neuralnets.widgets

/**
 * The one "interesting" target function shared by the Riemann-sum and universal-approximation
 * widgets, so the deck can tell one continuous story: first approximate its area with rectangles,
 * then approximate the *function itself* with a network built from rectangle-shaped "pulses".
 */
object ApproxTarget {
  val xMin = 0.0
  val xMax = 6.0
  val yMin = -0.3
  val yMax = 2.6

  /** A wobbly bump with a dip, chosen to need several rectangles/pulses before it looks right. */
  def f(x: Double): Double =
    1.0 + math.sin(x) + 0.6 * math.sin(2.6 * x + 1.0)

  /** Numeric definite integral of `f` over [xMin, xMax], via a fine midpoint sum - used as ground truth. */
  def trueIntegral(steps: Int = 20000): Double =
    val dx = (xMax - xMin) / steps
    (0 until steps).map { i => f(xMin + (i + 0.5) * dx) * dx }.sum
}
