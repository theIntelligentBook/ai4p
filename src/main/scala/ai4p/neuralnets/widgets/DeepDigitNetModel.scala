package ai4p.neuralnets.widgets

import scala.util.Random

/**
 * The same three digits as [[TinyDigitNetModel]], but this time asking a harder question: can the
 * network still recognise a digit if it's drawn a little differently, or nudged off-centre?
 * A single hidden layer has to map pixels almost directly to a digit; with two hidden layers,
 * the first can learn small local patterns (roughly "is there a stroke around here"), and the
 * second can learn *relationships between those patterns* ("this arrangement of strokes, wherever
 * exactly it sits, is a loop") - relationships between the first layer's relationships, and one
 * step closer to genuine shape recognition rather than pixel memorisation.
 */
object DeepDigitNetModel {
  import TinyDigitNetModel.{gridSize, nPixels}

  val nHidden1 = 10
  val nHidden2 = 6
  val nOut = TinyDigitNetModel.templates.size

  private def parse(rows: String): Vector[Int] =
    rows.linesIterator.flatMap(_.map(c => if c == '1' then 1 else 0)).toVector

  /** One way of drawing a digit - `label` indexes into [[TinyDigitNetModel.templates]] (so it
    * lines up with the network's output neurons), `variant` just describes it for the UI. */
  case class Shape(label: Int, digitName: String, variant: String, pixels: Vector[Int])

  /** Each digit, drawn more than one way - a size variant and (for '4') a structural variant -
    * so "what makes this a 0" has to come from more than one fixed arrangement of pixels. */
  val shapeLibrary: Vector[Shape] = Vector(
    Shape(0, "0", "standard", TinyDigitNetModel.templates(0).pixels),
    Shape(0, "0", "smaller", parse(
      """00000
        |01110
        |01010
        |01110
        |00000""".stripMargin)),

    Shape(1, "1", "standard", TinyDigitNetModel.templates(1).pixels),
    Shape(1, "1", "smaller", parse(
      """00000
        |00100
        |00100
        |00100
        |00000""".stripMargin)),

    // "closed top" draws a single diagonal stroke from the top down to the crossbar (like most
    // handwriting); "open top" is the seven-segment/calculator style instead - separate left and
    // right verticals that don't meet at the top at all, joined only by the middle crossbar.
    Shape(2, "4", "closed top", TinyDigitNetModel.templates(2).pixels),
    Shape(2, "4", "open top", parse(
      """01010
        |01010
        |01110
        |00010
        |00010""".stripMargin))
  )

  /** Shifts a gridSize x gridSize pixel pattern by (dx, dy); cells that would come from off the
    * edge of the grid are treated as 0 (blank), so a shifted digit can partially run off the grid. */
  def shift(pixels: Vector[Int], dx: Int, dy: Int): Vector[Int] =
    Vector.tabulate(nPixels) { idx =>
      val row = idx / gridSize; val col = idx % gridSize
      val sr = row - dy; val sc = col - dx
      if sr >= 0 && sr < gridSize && sc >= 0 && sc < gridSize then pixels(sr * gridSize + sc) else 0
    }

  case class Example(label: Int, name: String, variant: String, dx: Int, dy: Int, pixels: Vector[Int])

  /** The full 3x3 neighbourhood of one-pixel nudges (including diagonals) - wider coverage than
    * just the four cardinal directions, so testing a diagonal nudge isn't automatically "unseen". */
  val trainingOffsets: Vector[(Int, Int)] =
    (for dx <- -1 to 1; dy <- -1 to 1 yield (dx, dy)).toVector

  /** Every shape in the library, nudged by every training offset. */
  val trainingSet: Vector[Example] =
    shapeLibrary.flatMap { s =>
      trainingOffsets.map { (dx, dy) => Example(s.label, s.digitName, s.variant, dx, dy, shift(s.pixels, dx, dy)) }
    }
}

/** A 25 -> nHidden1 -> nHidden2 -> 3 network - the same learning mechanism as [[TinyDigitNet]],
  * just one hidden layer deeper, and trained on [[DeepDigitNetModel.trainingSet]]'s varied digits. */
class DeepDigitNet(seed: Long = 7L) {
  import TinyDigitNetModel.{nPixels, sigmoid, softmax, argmax}
  import DeepDigitNetModel._

  private val rnd = new Random(seed)

  /** Xavier-ish initialisation, scaled down by fan-in - keeps the very first layer's weighted
    * sums small enough that sigmoid doesn't saturate immediately, which otherwise stalls
    * backprop before it gets anywhere (the deeper the network, the worse this bites). */
  private def initWeight(fanIn: Int): Double = (rnd.nextDouble() - 0.5) * 2.0 / math.sqrt(fanIn)

  var w1: Array[Array[Double]] = Array.fill(nPixels, nHidden1)(initWeight(nPixels))
  var b1: Array[Double] = Array.fill(nHidden1)(0.0)
  var w2: Array[Array[Double]] = Array.fill(nHidden1, nHidden2)(initWeight(nHidden1))
  var b2: Array[Double] = Array.fill(nHidden2)(0.0)
  var w3: Array[Array[Double]] = Array.fill(nHidden2, nOut)(initWeight(nHidden2))
  var b3: Array[Double] = Array.fill(nOut)(0.0)
  var epoch: Int = 0

  case class Forward(a1: Vector[Double], a2: Vector[Double], a3: Vector[Double])
  private case class Grads(
    gw1: Array[Array[Double]], gb1: Array[Double],
    gw2: Array[Array[Double]], gb2: Array[Double],
    gw3: Array[Array[Double]], gb3: Array[Double]
  )

  def forward(x: Vector[Int]): Forward =
    val z1 = Array.tabulate(nHidden1)(j => (0 until nPixels).map(i => x(i) * w1(i)(j)).sum + b1(j))
    val a1 = z1.map(sigmoid).toVector
    val z2 = Array.tabulate(nHidden2)(k => (0 until nHidden1).map(j => a1(j) * w2(j)(k)).sum + b2(k))
    val a2 = z2.map(sigmoid).toVector
    val z3 = Array.tabulate(nOut)(o => (0 until nHidden2).map(k => a2(k) * w3(k)(o)).sum + b3(o))
    val a3 = softmax(z3.toVector)
    Forward(a1, a2, a3)

  def loss(): Double =
    trainingSet.map { ex =>
      val f = forward(ex.pixels)
      -math.log(math.max(1e-9, f.a3(ex.label)))
    }.sum / trainingSet.size

  def correctCount(): Int =
    trainingSet.count(ex => argmax(forward(ex.pixels).a3) == ex.label)

  /** The cross-entropy gradient w.r.t. every weight/bias, for a single training example. */
  private def gradsFor(ex: Example): Grads =
    val x = ex.pixels
    val f = forward(x)
    val dz3 = Array.tabulate(nOut)(o => f.a3(o) - (if o == ex.label then 1.0 else 0.0))
    val gw3 = Array.tabulate(nHidden2, nOut)((k, o) => f.a2(k) * dz3(o))

    val da2 = Array.tabulate(nHidden2)(k => (0 until nOut).map(o => dz3(o) * w3(k)(o)).sum)
    val dz2 = Array.tabulate(nHidden2)(k => da2(k) * f.a2(k) * (1 - f.a2(k)))
    val gw2 = Array.tabulate(nHidden1, nHidden2)((j, k) => f.a1(j) * dz2(k))

    val da1 = Array.tabulate(nHidden1)(j => (0 until nHidden2).map(k => dz2(k) * w2(j)(k)).sum)
    val dz1 = Array.tabulate(nHidden1)(j => da1(j) * f.a1(j) * (1 - f.a1(j)))
    val gw1 = Array.tabulate(nPixels, nHidden1)((i, j) => x(i) * dz1(j))

    Grads(gw1, dz1, gw2, dz2, gw3, dz3)

  private def applyGrads(g: Grads, lr: Double): Unit =
    for i <- 0 until nPixels; j <- 0 until nHidden1 do w1(i)(j) -= lr * g.gw1(i)(j)
    for j <- 0 until nHidden1 do b1(j) -= lr * g.gb1(j)
    for j <- 0 until nHidden1; k <- 0 until nHidden2 do w2(j)(k) -= lr * g.gw2(j)(k)
    for k <- 0 until nHidden2 do b2(k) -= lr * g.gb2(k)
    for k <- 0 until nHidden2; o <- 0 until nOut do w3(k)(o) -= lr * g.gw3(k)(o)
    for o <- 0 until nOut do b3(o) -= lr * g.gb3(o)

  /** Adjusts the weights based on just one training example's error - the visible, one-example-at-
    * a-time update used by the widget's feed-forward/adjust-weights walkthrough. */
  def stepOne(idx: Int, lr: Double = 1.5): Unit =
    applyGrads(gradsFor(trainingSet(idx)), lr)
    epoch += 1

  /** One full-batch gradient-descent step, averaged over the whole training set - for fast training. */
  def step(lr: Double = 0.8): Unit =
    val all = trainingSet.map(gradsFor)
    val n = trainingSet.size.toDouble
    val gw1 = Array.tabulate(nPixels, nHidden1)((i, j) => all.map(_.gw1(i)(j)).sum / n)
    val gb1 = Array.tabulate(nHidden1)(j => all.map(_.gb1(j)).sum / n)
    val gw2 = Array.tabulate(nHidden1, nHidden2)((j, k) => all.map(_.gw2(j)(k)).sum / n)
    val gb2 = Array.tabulate(nHidden2)(k => all.map(_.gb2(k)).sum / n)
    val gw3 = Array.tabulate(nHidden2, nOut)((k, o) => all.map(_.gw3(k)(o)).sum / n)
    val gb3 = Array.tabulate(nOut)(o => all.map(_.gb3(o)).sum / n)
    applyGrads(Grads(gw1, gb1, gw2, gb2, gw3, gb3), lr)
    epoch += 1
}
