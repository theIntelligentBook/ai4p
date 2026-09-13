package ai4p.embeddings.widgets

import scala.util.Random

/**
 * A minimal skip-gram word2vec, trained live on the Peter Rabbit text itself. The corpus is far
 * too small and repetitive for this to learn anything as useful as the pretrained GloVe vectors
 * used elsewhere in this deck - that's rather the point: this is what training your own
 * embeddings actually looks like when you don't have enough text.
 */
object Word2VecModel {

  val windowSize = 3

  /** Every distinct word in the story, in the order it first appears. Word2vec conventionally
    * trains on the raw token stream, so (unlike [[LineVectorModel]]'s display vocabulary) this
    * keeps stopwords in - they're real context, even if they're not interesting to look up. */
  val vocab: Vector[String] =
    PeterRabbitModel.tokens.flatten.map(PeterRabbitModel.keyOf).filter(_.nonEmpty).distinct

  val indexOf: Map[String, Int] = vocab.zipWithIndex.toMap

  /** (center, context) index pairs from a sliding window over each paragraph's own token sequence
    * - paragraphs don't run into each other, so a window never crosses that boundary. */
  val pairs: Vector[(Int, Int)] =
    PeterRabbitModel.tokens.flatMap { lineTokens =>
      val idxs = lineTokens.map(PeterRabbitModel.keyOf).filter(_.nonEmpty).map(indexOf)
      for
        i <- idxs.indices
        j <- math.max(0, i - windowSize) to math.min(idxs.size - 1, i + windowSize)
        if j != i
      yield (idxs(i), idxs(j))
    }

  // Training hammers these tens of thousands of times a second, so they're kept as plain
  // primitive arrays (rather than re-reading `pairs`' boxed tuples every step).
  private[widgets] val pairCenters: Array[Int] = pairs.map(_._1).toArray
  private[widgets] val pairContexts: Array[Int] = pairs.map(_._2).toArray

  /** Softmax-with-max-subtraction, overwriting `z` in place (and returning it) rather than
    * allocating a fresh array - it runs once per training pair, tens of thousands of times a
    * second, so this is written as plain loops (no closures/boxing) too. */
  def softmax(z: Array[Double]): Array[Double] =
    val n = z.length
    var m = Double.NegativeInfinity
    var i = 0
    while i < n do
      if z(i) > m then m = z(i)
      i += 1
    var total = 0.0
    i = 0
    while i < n do
      val e = math.exp(z(i) - m)
      z(i) = e
      total += e
      i += 1
    i = 0
    while i < n do
      z(i) = z(i) / total
      i += 1
    z
}

/** The trainable network: two `vocab.size x embDim` weight matrices (a "centre" embedding, used
  * when a word is the one being looked at, and a "context" embedding, used when it's a neighbour
  * being predicted), updated by plain SGD on skip-gram softmax loss. Only a couple of thousand
  * parameters at embDim=2, so a full epoch over the whole corpus is fast - so long as the per-pair
  * work is plain array loops rather than allocating collections, since it runs once per training
  * pair (thousands of times) every epoch.
  */
class TinyWord2Vec(embDim: Int = 2, seed: Long = 42L) {
  import Word2VecModel._

  private val rnd = new Random(seed)
  val vocabSize: Int = vocab.size

  var center: Array[Array[Double]] = Array.fill(vocabSize, embDim)((rnd.nextDouble() - 0.5) * 0.2)
  var context: Array[Array[Double]] = Array.fill(vocabSize, embDim)((rnd.nextDouble() - 0.5) * 0.2)
  var epoch: Int = 0

  private val z = new Array[Double](vocabSize)
  private val dz = new Array[Double](vocabSize)
  private val dh = new Array[Double](embDim)

  /** One SGD step on a single (center, context) pair; returns that example's loss. Reuses the
    * `z`/`dz`/`dh` scratch arrays above rather than allocating fresh ones every call. */
  private def stepOne(centerIdx: Int, contextIdx: Int, lr: Double): Double =
    val h = center(centerIdx)

    var j = 0
    while j < vocabSize do
      val cj = context(j)
      var s = 0.0
      var k = 0
      while k < embDim do
        s += h(k) * cj(k)
        k += 1
      z(j) = s
      j += 1
    val probs = softmax(z)
    val loss = -math.log(math.max(1e-9, probs(contextIdx)))

    j = 0
    while j < vocabSize do
      dz(j) = probs(j) - (if j == contextIdx then 1.0 else 0.0)
      j += 1

    var k = 0
    while k < embDim do
      dh(k) = 0.0
      k += 1
    j = 0
    while j < vocabSize do
      val dzj = dz(j)
      val cj = context(j)
      k = 0
      while k < embDim do
        dh(k) += dzj * cj(k)
        k += 1
      j += 1

    // Context rows are updated using `h`'s current (pre-update) values, so this has to happen
    // before `center(centerIdx)` (the same array `h` points to) is changed below.
    j = 0
    while j < vocabSize do
      val dzj = dz(j)
      val cj = context(j)
      k = 0
      while k < embDim do
        cj(k) -= lr * dzj * h(k)
        k += 1
      j += 1

    k = 0
    while k < embDim do
      h(k) -= lr * dh(k)
      k += 1

    loss

  /** One epoch over every training pair, in a freshly shuffled order; returns the average loss. */
  def trainEpoch(lr: Double = 0.05): Double =
    val n = pairCenters.length
    val order = Array.tabulate(n)(identity)
    var i = n - 1
    while i > 0 do
      val j = rnd.nextInt(i + 1)
      val tmp = order(i); order(i) = order(j); order(j) = tmp
      i -= 1

    var total = 0.0
    i = 0
    while i < n do
      val p = order(i)
      total += stepOne(pairCenters(p), pairContexts(p), lr)
      i += 1
    epoch += 1
    total / n
}
