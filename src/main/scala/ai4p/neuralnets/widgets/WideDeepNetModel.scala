package ai4p.neuralnets.widgets

import scala.util.Random

/**
 * A regression network with a small, fixed width (wider than [[DeepNarrowNetModel]]'s strict two
 * neurons, but still narrow compared to the pulses widget's single wide hidden layer), trained by
 * backpropagation *with momentum* to approximate the same wobbly target function as the pulses
 * widget - showing that depth-driven universal approximation works with a modest width too, given
 * real training, not just a hand-derived closed form for a function simple enough to have one.
 *
 * Plain gradient descent on a narrow, fairly deep network is a genuinely hard optimisation problem:
 * with a leaky-ReLU slope too close to zero, the backward-flowing gradient gets multiplied by that
 * slope at every layer a unit happens to be "off" for, so it can shrink to almost nothing by the
 * time it reaches the early layers - and the easiest way to reduce error with no usable gradient
 * on the input-dependent weights is to just settle the output bias on the target's average, i.e.
 * a flat line. A larger leak (0.2, not a token 0.05) and momentum (which keeps pushing a weight in
 * a consistently-useful direction even while its instantaneous gradient is small) are the standard
 * fixes, and both are used here.
 */
object WideDeepNetModel {
  val width = 4

  /** Leaky ReLU: unlike a "barely leaky" version, a slope of 0.2 keeps backprop's gradient from
    * vanishing too fast as it passes through several "off" units on its way back through depth. */
  def activation(x: Double): Double = if x > 0 then x else 0.2 * x
}

class WideDeepNet(depth: Int, seed: Long = 11L) {
  import WideDeepNetModel._
  import ApproxTarget.{xMin, xMax, f}

  /** 1 (input) -> width, width, ... (depth hidden layers) -> 1 (output). */
  val layerSizes: Vector[Int] = Vector(1) ++ Vector.fill(depth)(width) ++ Vector(1)
  private val nLayers = layerSizes.size - 1

  private val rnd = new Random(seed)
  private def initLayer(fanIn: Int, fanOut: Int): Array[Array[Double]] =
    Array.fill(fanIn, fanOut)((rnd.nextDouble() - 0.5) * (1.0 / math.sqrt(fanIn)))

  var weights: Array[Array[Array[Double]]] = Array.tabulate(nLayers)(l => initLayer(layerSizes(l), layerSizes(l + 1)))
  var biases: Array[Array[Double]] = Array.tabulate(nLayers)(l => Array.fill(layerSizes(l + 1))(0.0))
  var epoch: Int = 0

  // Momentum buffers - same shapes as weights/biases, carried across steps.
  private val velocityW: Array[Array[Array[Double]]] = Array.tabulate(nLayers)(l => Array.fill(layerSizes(l), layerSizes(l + 1))(0.0))
  private val velocityB: Array[Array[Double]] = Array.tabulate(nLayers)(l => Array.fill(layerSizes(l + 1))(0.0))

  /** Evenly-spaced points across the target's domain, used both to measure and to train on. */
  val samples: Vector[Double] = Vector.tabulate(40)(i => xMin + (xMax - xMin) * i / 39.0)

  // Start the output at the target's average, rather than at zero - a flat line is a reasonable
  // *starting* guess, it just shouldn't be where training ends up.
  biases(nLayers - 1)(0) = samples.map(f).sum / samples.size

  private def normalize(x: Double): Double = (x - xMin) / (xMax - xMin)

  /** Every layer's activations for input `x`, from the normalised input to the final scalar output
    * (linear, no activation on the last layer, since this is regression). */
  def activations(x: Double): Vector[Vector[Double]] =
    var current = Vector(normalize(x))
    val acts = scala.collection.mutable.ArrayBuffer(current)
    for l <- 0 until nLayers do
      val w = weights(l); val b = biases(l)
      val outSize = layerSizes(l + 1)
      val z = Vector.tabulate(outSize)(j => (0 until w.length).map(i => current(i) * w(i)(j)).sum + b(j))
      current = if l == nLayers - 1 then z else z.map(activation)
      acts += current
    acts.toVector

  def predict(x: Double): Double = activations(x).last.head

  def loss(): Double = samples.map(x => math.pow(predict(x) - f(x), 2)).sum / samples.size

  private case class Grads(w: Array[Array[Array[Double]]], b: Array[Array[Double]])

  /** Backprop for one sample point, minimising squared error against the target function. */
  private def gradsFor(x: Double): Grads =
    val acts = activations(x)
    val deltas = new Array[Vector[Double]](nLayers)
    deltas(nLayers - 1) = Vector(2.0 * (acts.last.head - f(x)))
    for l <- nLayers - 2 to 0 by -1 do
      val wNext = weights(l + 1)
      val actNext = acts(l + 1)
      val dNext = deltas(l + 1)
      deltas(l) = Vector.tabulate(wNext.length) { j =>
        val upstream = (0 until dNext.size).map(k => dNext(k) * wNext(j)(k)).sum
        upstream * (if actNext(j) > 0 then 1.0 else 0.2)
      }
    val gw = Array.tabulate(nLayers)(l => Array.tabulate(layerSizes(l), layerSizes(l + 1))((i, j) => acts(l)(i) * deltas(l)(j)))
    val gb = Array.tabulate(nLayers)(l => deltas(l).toArray)
    Grads(gw, gb)

  /** Classical momentum: `v <- momentum*v - lr*grad; w <- w + v`. Keeps a weight moving in a
    * consistently-useful direction even on steps where its instantaneous gradient is tiny -
    * exactly the situation a deep, narrow, leaky-ReLU network runs into constantly. */
  private def applyGrads(g: Grads, lr: Double, momentum: Double): Unit =
    for l <- 0 until nLayers do
      for i <- 0 until layerSizes(l); j <- 0 until layerSizes(l + 1) do
        velocityW(l)(i)(j) = momentum * velocityW(l)(i)(j) - lr * g.w(l)(i)(j)
        weights(l)(i)(j) += velocityW(l)(i)(j)
      for j <- 0 until layerSizes(l + 1) do
        velocityB(l)(j) = momentum * velocityB(l)(j) - lr * g.b(l)(j)
        biases(l)(j) += velocityB(l)(j)

  /** One full-batch gradient-descent (with momentum) step, averaged over every sample point. */
  def step(lr: Double = 0.05, momentum: Double = 0.9): Unit =
    val all = samples.map(gradsFor)
    val n = samples.size.toDouble
    val gw = Array.tabulate(nLayers)(l => Array.tabulate(layerSizes(l), layerSizes(l + 1))((i, j) => all.map(_.w(l)(i)(j)).sum / n))
    val gb = Array.tabulate(nLayers)(l => Array.tabulate(layerSizes(l + 1))(j => all.map(_.b(l)(j)).sum / n))
    applyGrads(Grads(gw, gb), lr, momentum)
    epoch += 1
}
