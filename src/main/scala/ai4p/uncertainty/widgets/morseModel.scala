package ai4p.uncertainty.widgets

import scala.util.Random
import scala.collection.mutable.ArrayBuffer

/**
 * A small, deliberately simplified "toy Morse code" shared by the two Morse widgets. Real Morse
 * uses variable-length codes (E is a single dot, O is three dashes), which makes segmenting a
 * noisy signal into letters a whole separate problem on its own. To keep the focus squarely on
 * how a Hidden Markov Model uses context to fix noisy *symbols* — rather than also having to
 * solve segmentation — every letter here gets exactly three dot/dash symbols, using all eight
 * possible 3-symbol patterns for an eight-letter alphabet.
 */
object MorseModel {

  /** A symbol is a dot (`false`) or a dash (`true`). */
  type Code = Vector[Boolean]

  val codeLength = 3

  case class Letter(char: Char, code: Code)

  val letters: Vector[Letter] = Vector(
    Letter('E', Vector(false, false, false)),
    Letter('T', Vector(true, true, true)),
    Letter('A', Vector(false, true, false)),
    Letter('O', Vector(true, true, false)),
    Letter('N', Vector(true, false, true)),
    Letter('S', Vector(false, true, true)),
    Letter('I', Vector(false, false, true)),
    Letter('H', Vector(true, false, false))
  )

  val n: Int = letters.size

  def codeString(code: Code): String = code.map(d => if d then "-" else ".").mkString

  private def normalizeRows(rows: Vector[Vector[Double]]): Vector[Vector[Double]] =
    rows.map(r => r.map(_ / r.sum))

  /**
   * A hand-authored letter-transition model, loosely inspired by common English digraphs within
   * this small alphabet: T→H (as in "the"/"this"/"that"), H→E ("he"), A→N ("an"/"and"), O→N
   * ("on"), N→O ("no"/"not"), S→T ("st"). Everything else is left roughly uniform. Rows sum to 1.
   */
  val transition: Vector[Vector[Double]] = normalizeRows(Vector(
    //     E,   T,   A,   O,   N,   S,   I,   H
    Vector(1.0, 1.0, 1.0, 1.0, 1.0, 3.0, 1.0, 3.0), // from E
    Vector(1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 6.0), // from T — strong T→H
    Vector(1.0, 1.0, 1.0, 1.0, 5.0, 1.0, 1.0, 1.0), // from A — A→N
    Vector(1.0, 1.0, 1.0, 1.0, 5.0, 1.0, 1.0, 1.0), // from O — O→N
    Vector(1.0, 1.0, 1.0, 4.0, 1.0, 1.0, 1.0, 1.0), // from N — N→O
    Vector(1.0, 4.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0), // from S — S→T
    Vector(1.0, 1.0, 1.0, 1.0, 5.0, 1.0, 1.0, 1.0), // from I — I→N
    Vector(5.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0)  // from H — H→E
  ))

  val start: Vector[Double] = Vector.fill(n)(1.0 / n)

  val letterIndex: Map[Char, Int] = letters.zipWithIndex.map((l, i) => l.char -> i).toMap

  /** Turns a string into indices into `letters`, silently dropping any character outside this toy alphabet. */
  def parseMessage(s: String): Vector[Int] = s.toUpperCase.flatMap(c => letterIndex.get(c)).toVector

  def messageString(seq: Vector[Int]): String = seq.map(i => letters(i).char).mkString

  def sampleIndex(probs: Vector[Double], rng: Random): Int =
    val r = rng.nextDouble()
    var cum = 0.0
    var i = 0
    while i < probs.size - 1 && { cum += probs(i); r > cum } do i += 1
    i

  /** Samples a random sequence of `length` letters (as indices into `letters`) from the Markov chain. */
  def generate(length: Int, rng: Random = new Random()): Vector[Int] =
    val out = ArrayBuffer.empty[Int]
    var state = sampleIndex(start, rng)
    out += state
    for _ <- 1 until length do
      state = sampleIndex(transition(state), rng)
      out += state
    out.toVector

  /** Flips each symbol in `code` independently with probability `noiseP`. */
  def applyNoise(code: Code, noiseP: Double, rng: Random): Code =
    code.map(bit => if rng.nextDouble() < noiseP then !bit else bit)

  def hammingDistance(a: Code, b: Code): Int = a.zip(b).count((x, y) => x != y)

  /** P(observed | true letter) under independent per-symbol flips with probability `noiseP`. */
  def emissionLikelihood(observed: Code, letterIdx: Int, noiseP: Double): Double =
    val d = hammingDistance(observed, letters(letterIdx).code)
    math.pow(noiseP, d) * math.pow(1 - noiseP, codeLength - d)

  /** The best-matching letter at each position, considered independently — no context from neighbouring letters. */
  def naiveDecode(observed: Vector[Code], noiseP: Double): Vector[Int] =
    observed.map(obs => (0 until n).maxBy(i => emissionLikelihood(obs, i, noiseP)))

  /**
   * Viterbi decoding: the single most likely *sequence* of letters given the noisy observations,
   * combining the transition model (what letter sequences are plausible) with the emission model
   * (how well each observed symbol pattern matches each letter) — using context from neighbouring
   * letters to resolve ambiguous or corrupted symbols that a per-position decode can't.
   */
  def viterbiDecode(observed: Vector[Code], noiseP: Double): Vector[Int] =
    if observed.isEmpty then Vector.empty
    else
      val t = observed.size
      val logProb = Array.ofDim[Double](t, n)
      val backptr = Array.ofDim[Int](t, n)

      for s <- 0 until n do
        logProb(0)(s) = math.log(start(s)) + math.log(emissionLikelihood(observed(0), s, noiseP))

      for pos <- 1 until t do
        for s <- 0 until n do
          var bestPrev = 0
          var bestScore = Double.NegativeInfinity
          for prev <- 0 until n do
            val score = logProb(pos - 1)(prev) + math.log(transition(prev)(s))
            if score > bestScore then
              bestScore = score
              bestPrev = prev
          logProb(pos)(s) = bestScore + math.log(emissionLikelihood(observed(pos), s, noiseP))
          backptr(pos)(s) = bestPrev

      val path = ArrayBuffer.fill(t)(0)
      path(t - 1) = (0 until n).maxBy(s => logProb(t - 1)(s))
      for pos <- (t - 2) to 0 by -1 do
        path(pos) = backptr(pos + 1)(path(pos + 1))
      path.toVector
}
