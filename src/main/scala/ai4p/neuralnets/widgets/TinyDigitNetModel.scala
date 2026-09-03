package ai4p.neuralnets.widgets

import scala.util.Random

/**
 * A tiny pixel-grid "digit" classifier: a handful of low-resolution digit shapes, and a small
 * 25 -> hidden -> 3 feedforward network plain enough to retrain with visible backprop on every click.
 */
object TinyDigitNetModel {
  val gridSize = 5
  val nPixels = gridSize * gridSize
  val nHidden = 8

  case class Template(name: String, pixels: Vector[Int])

  private def parse(rows: String): Vector[Int] =
    rows.linesIterator.flatMap(_.map(c => if c == '1' then 1 else 0)).toVector

  /** Deliberately simplified, blocky 5x5 sketches - just enough detail to tell the three shapes apart. */
  val templates: Vector[Template] = Vector(
    Template("0", parse(
      """01110
        |10001
        |10001
        |10001
        |01110""".stripMargin)),
    Template("1", parse(
      """00100
        |01100
        |00100
        |00100
        |01110""".stripMargin)),
    Template("4", parse(
      """00010
        |00110
        |01010
        |11111
        |00010""".stripMargin))
  )

  def sigmoid(x: Double): Double = 1.0 / (1.0 + math.exp(-x))

  def softmax(v: Vector[Double]): Vector[Double] =
    val m = v.max
    val exps = v.map(x => math.exp(x - m))
    val total = exps.sum
    exps.map(_ / total)

  def argmax(v: Vector[Double]): Int = v.zipWithIndex.maxBy(_._1)._2
}

/** A 25 -> nHidden -> 3 network, trained by plain full-batch gradient descent on cross-entropy
  * loss over the fixed pixel [[TinyDigitNetModel.templates]]. Small enough to retrain live in-browser. */
class TinyDigitNet(seed: Long = 42L) {
  import TinyDigitNetModel._

  private val rnd = new Random(seed)
  private val nOut = templates.size

  var w1: Array[Array[Double]] = Array.fill(nPixels, nHidden)((rnd.nextDouble() - 0.5) * 0.8)
  var b1: Array[Double] = Array.fill(nHidden)(0.0)
  var w2: Array[Array[Double]] = Array.fill(nHidden, nOut)((rnd.nextDouble() - 0.5) * 0.8)
  var b2: Array[Double] = Array.fill(nOut)(0.0)
  var epoch: Int = 0

  case class Forward(a1: Vector[Double], a2: Vector[Double])

  def forward(x: Vector[Int]): Forward =
    val z1 = Array.tabulate(nHidden)(j => (0 until nPixels).map(i => x(i) * w1(i)(j)).sum + b1(j))
    val a1 = z1.map(sigmoid).toVector
    val z2 = Array.tabulate(nOut)(k => (0 until nHidden).map(j => a1(j) * w2(j)(k)).sum + b2(k))
    val a2 = softmax(z2.toVector)
    Forward(a1, a2)

  def loss(): Double =
    templates.zipWithIndex.map { case (t, idx) =>
      val f = forward(t.pixels)
      -math.log(math.max(1e-9, f.a2(idx)))
    }.sum / templates.size

  def correctCount(): Int =
    templates.zipWithIndex.count { case (t, idx) => argmax(forward(t.pixels).a2) == idx }

  private case class Grads(gw1: Array[Array[Double]], gb1: Array[Double], gw2: Array[Array[Double]], gb2: Array[Double])

  /** The cross-entropy gradient w.r.t. every weight/bias, for a single template. Shared by the
    * one-example-at-a-time [[stepOne]] (used by the widget's feed-forward/adjust-weights walkthrough)
    * and the full-batch [[step]] (used for fast training). */
  private def gradsFor(idx: Int): Grads =
    val x = templates(idx).pixels
    val f = forward(x)
    val dz2 = Array.tabulate(nOut)(k => f.a2(k) - (if k == idx then 1.0 else 0.0))
    val gw2 = Array.tabulate(nHidden, nOut)((j, k) => f.a1(j) * dz2(k))
    val da1 = Array.tabulate(nHidden)(j => (0 until nOut).map(k => dz2(k) * w2(j)(k)).sum)
    val dz1 = Array.tabulate(nHidden)(j => da1(j) * f.a1(j) * (1 - f.a1(j)))
    val gw1 = Array.tabulate(nPixels, nHidden)((i, j) => x(i) * dz1(j))
    Grads(gw1, dz1, gw2, dz2)

  private def applyGrads(g: Grads, lr: Double): Unit =
    for i <- 0 until nPixels; j <- 0 until nHidden do w1(i)(j) -= lr * g.gw1(i)(j)
    for j <- 0 until nHidden do b1(j) -= lr * g.gb1(j)
    for j <- 0 until nHidden; k <- 0 until nOut do w2(j)(k) -= lr * g.gw2(j)(k)
    for k <- 0 until nOut do b2(k) -= lr * g.gb2(k)

  /** Adjusts the weights based on just *one* template's error - the visible, single-example update
    * used by the "feed forward, then adjust weights" walkthrough. */
  def stepOne(idx: Int, lr: Double = 1.0): Unit =
    applyGrads(gradsFor(idx), lr)
    epoch += 1

  /** One full-batch gradient-descent step, averaged over all templates - used for fast training. */
  def step(lr: Double = 0.5): Unit =
    val all = templates.indices.map(gradsFor)
    val n = templates.size.toDouble
    val gw1 = Array.tabulate(nPixels, nHidden)((i, j) => all.map(_.gw1(i)(j)).sum / n)
    val gb1 = Array.tabulate(nHidden)(j => all.map(_.gb1(j)).sum / n)
    val gw2 = Array.tabulate(nHidden, nOut)((j, k) => all.map(_.gw2(j)(k)).sum / n)
    val gb2 = Array.tabulate(nOut)(k => all.map(_.gb2(k)).sum / n)
    applyGrads(Grads(gw1, gb1, gw2, gb2), lr)
    epoch += 1
}
