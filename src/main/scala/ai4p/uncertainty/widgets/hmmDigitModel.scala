package ai4p.uncertainty.widgets

import scala.collection.mutable.ArrayBuffer

/**
 * A small, deliberately toy Hidden Markov Model for recognising a hand-drawn digit (0-9) traced as
 * a *single* stroke (mouse down to mouse up).
 *
 * The pipeline:
 *
 *   1. The raw mouse path is resampled to fixed-length steps, and each step between resampled
 *      points is bucketed into one of 8 compass directions — this is the observation alphabet
 *      (a classic "chain code"). Consecutive repeats are collapsed, so the observation sequence is
 *      really "the sequence of strokes": the run of distinct direction-segments in the drawing.
 *   2. Each digit has its own small left-to-right (Bakis) HMM: hidden state `i` is "currently
 *      drawing segment `i` of this digit's canonical shape", which emits the segment's expected
 *      compass direction (with some spread to neighbouring directions, since real strokes wobble).
 *   3. Viterbi decoding against each of the 10 per-digit HMMs gives both a log-likelihood (used to
 *      rank the 10 digits) and the most likely *state path* — i.e. which drawn stroke was "meant"
 *      to be which segment of the winning digit's shape.
 */
object HmmDigitModel {

  // --- The 8-direction chain-code alphabet -----------------------------------------------------

  /** Unit vectors for chain-code directions 0..7, going clockwise from due east (screen coordinates, y downwards). */
  val unitVectors: Vector[(Double, Double)] = Vector(
    (1.0, 0.0), (0.7071, 0.7071), (0.0, 1.0), (-0.7071, 0.7071),
    (-1.0, 0.0), (-0.7071, -0.7071), (0.0, -1.0), (0.7071, -0.7071)
  )

  val glyphs: Vector[String] = Vector("→", "↘", "↓", "↙", "←", "↖", "↑", "↗")

  def circularDist(a: Int, b: Int): Int =
    val d = math.abs(a - b) % 8
    math.min(d, 8 - d)

  private val concentration = 0.28
  private val rawWeightByDist: Vector[Double] = Vector.tabulate(5)(d => math.pow(concentration, d))
  private val multiplicityByDist: Vector[Int] = Vector(1, 2, 2, 2, 1)
  private val normalizer: Double = multiplicityByDist.zip(rawWeightByDist).map { case (m, w) => m * w }.sum

  /** P(observed direction | a state expecting `expected`) — a discrete, circular, bell-shaped kernel over the 8 directions. */
  def emissionProb(observed: Int, expected: Int): Double =
    rawWeightByDist(circularDist(observed, expected)) / normalizer

  def bucketOf(dx: Double, dy: Double): Int =
    val angle = math.atan2(dy, dx) * 180.0 / math.Pi
    val norm = ((angle % 360) + 360) % 360
    math.round(norm / 45.0).toInt % 8

  /** Walks a polyline, dropping a point every `step` pixels of arc length. */
  def resample(path: Vector[(Double, Double)], step: Double): Vector[(Double, Double)] =
    if path.size < 2 then path
    else
      val out = ArrayBuffer(path.head)
      var prevPoint = path.head
      var pending = step
      var i = 1
      while i < path.size do
        val next = path(i)
        val dx = next._1 - prevPoint._1
        val dy = next._2 - prevPoint._2
        val segLen = math.hypot(dx, dy)
        if segLen < pending then
          pending -= segLen
          prevPoint = next
          i += 1
        else
          val t = pending / segLen
          val sampled = (prevPoint._1 + dx * t, prevPoint._2 + dy * t)
          out += sampled
          prevPoint = sampled
          pending = step
      out.toVector

  /** Turns a raw mouse path into the "sequence of strokes" — distinct chain-code direction segments. */
  def toDirections(path: Vector[(Double, Double)], step: Double = 10.0): Vector[Int] =
    val sampled = resample(path, step)
    if sampled.size < 2 then Vector.empty
    else
      val raw = sampled.sliding(2).collect { case Seq((x1, y1), (x2, y2)) => bucketOf(x2 - x1, y2 - y1) }.toVector
      raw.foldLeft(Vector.empty[Int])((acc, d) => if acc.lastOption.contains(d) then acc else acc :+ d)

  // --- Per-digit canonical (unistroke) chain-code templates -------------------------------------

  case class DigitTemplate(digit: Int, chain: Vector[Int])

  /** Simplified single-stroke "how you'd sketch it without lifting the pen" shapes for 0-9. */
  val templates: Vector[DigitTemplate] = Vector(
    DigitTemplate(0, Vector(1, 3, 5, 7)),       // loop: ↘ ↙ ↖ ↗
    DigitTemplate(1, Vector(2)),                // ↓
    DigitTemplate(2, Vector(0, 1, 3, 0)),       // → ↘ ↙ →
    DigitTemplate(3, Vector(1, 4, 1)),          // ↘ ← ↘
    DigitTemplate(4, Vector(3, 0, 2)),          // ↙ → ↓
    DigitTemplate(5, Vector(4, 2, 0, 2, 4)),    // ← ↓ → ↓ ←
    DigitTemplate(6, Vector(3, 2, 1, 5)),       // ↙ ↓ ↘ ↖
    DigitTemplate(7, Vector(0, 3)),             // → ↙
    DigitTemplate(8, Vector(1, 5, 1, 5)),       // ↘ ↖ ↘ ↖
    DigitTemplate(9, Vector(1, 3, 5, 7, 2))     // ↘ ↙ ↖ ↗ ↓  (0's loop, plus a descender)
  )

  /** A little unistroke path (starting at the origin) tracing out a template, for drawing a legend icon. */
  def previewPoints(chain: Vector[Int]): Vector[(Double, Double)] =
    val pts = ArrayBuffer((0.0, 0.0))
    var cur = (0.0, 0.0)
    for d <- chain do
      val (ux, uy) = unitVectors(d)
      cur = (cur._1 + ux, cur._2 + uy)
      pts += cur
    pts.toVector

  // --- Per-digit left-to-right (Bakis) HMM, and Viterbi decoding --------------------------------

  /** Self / forward-1 / forward-2(skip) transition weights for a `k`-state left-to-right HMM. */
  def buildTransition(k: Int): Vector[Vector[Double]] =
    Vector.tabulate(k) { i =>
      if i == k - 1 then
        Vector.tabulate(k)(j => if j == i then 1.0 else 0.0) // absorbing final state
      else if i + 2 <= k - 1 then
        Vector.tabulate(k) { j => if j == i then 0.30 else if j == i + 1 then 0.55 else if j == i + 2 then 0.15 else 0.0 }
      else
        Vector.tabulate(k) { j => if j == i then 0.35 else if j == i + 1 then 0.65 else 0.0 }
    }

  case class ClassificationResult(template: DigitTemplate, logProb: Double, statePath: Vector[Int])

  /** Viterbi decoding of `obs` against a `k`-state left-to-right HMM whose states expect `chain`'s directions. */
  def viterbi(chain: Vector[Int], obs: Vector[Int]): (Double, Vector[Int]) =
    val k = chain.size
    val m = obs.size
    if m == 0 || k == 0 then (Double.NegativeInfinity, Vector.empty)
    else
      val transition = buildTransition(k)
      val logProb = Array.ofDim[Double](m, k)
      val backptr = Array.ofDim[Int](m, k)

      for s <- 0 until k do
        val startLog = if s == 0 then 0.0 else Double.NegativeInfinity
        logProb(0)(s) = startLog + math.log(emissionProb(obs(0), chain(s)))

      for t <- 1 until m do
        for s <- 0 until k do
          var bestPrev = 0
          var bestScore = Double.NegativeInfinity
          for prev <- 0 until k do
            val tp = transition(prev)(s)
            if tp > 0 then
              val score = logProb(t - 1)(prev) + math.log(tp)
              if score > bestScore then
                bestScore = score
                bestPrev = prev
          logProb(t)(s) = bestScore + math.log(emissionProb(obs(t), chain(s)))
          backptr(t)(s) = bestPrev

      val path = Array.ofDim[Int](m)
      path(m - 1) = (0 until k).maxBy(s => logProb(m - 1)(s))
      for t <- (m - 2) to 0 by -1 do
        path(t) = backptr(t + 1)(path(t + 1))

      (logProb(m - 1)(path(m - 1)), path.toVector)

  /** Scores `obs` against every digit's HMM, best (highest log-likelihood) first. */
  def classify(obs: Vector[Int]): Vector[ClassificationResult] =
    templates.map { t =>
      val (score, path) = viterbi(t.chain, obs)
      ClassificationResult(t, score, path)
    }.sortBy(-_.logProb)

  /** Turns log-likelihoods into a normalised "confidence" distribution across the 10 digits. */
  def confidences(results: Vector[ClassificationResult]): Vector[Double] =
    val scores = results.map(_.logProb)
    val top = scores.max
    val exps = scores.map(s => math.exp(s - top))
    val total = exps.sum
    exps.map(_ / total)
}
