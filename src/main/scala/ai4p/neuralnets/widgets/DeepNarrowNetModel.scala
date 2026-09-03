package ai4p.neuralnets.widgets

/**
 * A width-2, depth-D network that needs no training - its weights are an exact, hand-verified
 * closed form, not a fit. The key fact: composing a **triangle wave** with itself gives
 * `T_1, T_2, ...` such that
 *
 *     u(1-u) = sum_{k=1}^infinity T_k(u) / 4^k
 *
 * (checked by hand at u=0.25 and u=0.5, both come out exact). Since `(x-x0)^2` is just a rescaling
 * of `u(1-u)`, truncating this sum at depth D gives the *quadratic* Taylor term of `f` around the
 * domain's centre `x0`, accurate to about `4^-D` - genuinely exponentially closer with each extra
 * pair of neurons, not just "one more hand-chosen term" the way earlier widgets added detail.
 *
 * The target here is a plain quadratic (not the wobbly pulses-widget function) on purpose: this
 * network's construction is *only* ever going to converge on a parabola, so picking a target
 * that actually is one lets you see the convergence land on something exact, rather than "as
 * close as a parabola can get" to a shape it never could match. See [[WideDeepWidget]] for a
 * version that gives up the strict two-neuron width in exchange for chasing the real target.
 */
object DeepNarrowNetModel {
  import ApproxTarget.{xMin, xMax}

  val maxDepth = 7

  /** A plain quadratic over the same domain as the other widgets - chosen so this network's
    * built-in convergence to a parabola lands on an exact match, not just "as close as it gets". */
  def f(x: Double): Double =
    val c = (xMin + xMax) / 2.0
    0.12 * (x - c) * (x - c) + 0.4

  /** One period of a triangle wave over [0,1]. */
  def triangle(x: Double): Double = 1.0 - math.abs(2.0 * x - 1.0)

  def normalize(x: Double): Double = (x - xMin) / (xMax - xMin)

  val x0: Double = (xMin + xMax) / 2.0
  val span: Double = xMax - xMin

  // A numerical (central-difference) first and second derivative of f at x0 - accurate enough for
  // a smooth target, and avoids having to hand-differentiate `f` separately.
  private val h = 1e-4
  val a0: Double = f(x0)
  val a1: Double = (f(x0 + h) - f(x0 - h)) / (2 * h)
  val a2: Double = (f(x0 + h) - 2 * f(x0) + f(x0 - h)) / (h * h)

  /** H_0(x) = normalised x; H_k(x) = triangle(H_(k-1)(x)) - each extra composition is one more
    * term of the series above, generated purely by depth. */
  def harmonics(x: Double, count: Int): Vector[Double] =
    Vector.iterate(normalize(x), count)(triangle)

  /** The exact linear + constant part of the quadratic Taylor expansion, plus the constant that
    * "completes the square" so the correction terms below can be subtracted from it. No
    * approximation needed here - only the quadratic term is built up gradually, by depth. */
  def baseValue(x: Double): Double = a0 + a1 * (x - x0) + 0.125 * a2 * span * span

  /** The fixed weight applied to `H_(j-1)` when accumulating into `S_j` (j = 1..depth). j=1 just
    * carries the input forward without contributing (there's no k=0 term in the series above), so
    * a network of depth D effectively gets D-1 real correction terms. */
  def coefficient(j: Int): Double = if j <= 1 then 0.0 else -0.5 * a2 * span * span / math.pow(4, j - 1)

  /** The no-training approximation of `f`, using `depth` layers of the squaring identity to
    * refine the quadratic term. */
  def approx(depth: Int)(x: Double): Double =
    val hs = harmonics(x, depth) // H_0 .. H_(depth-1)
    var s = baseValue(x)
    for j <- 1 to depth do s += coefficient(j) * hs(j - 1)
    s
}
