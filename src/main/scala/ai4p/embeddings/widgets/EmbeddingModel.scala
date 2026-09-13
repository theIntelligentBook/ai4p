package ai4p.embeddings.widgets

import org.scalajs.dom
import scala.scalajs.js.Thenable.Implicits.thenable2future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * A small pretrained word-vector table (50-dimensional GloVe vectors, Pennington/Socher/Manning,
 * trained on Wikipedia + Gigaword) trimmed to its ~10,000 most frequent words, so it's light enough
 * to fetch and hold in a browser tab. It's not trained here - just loaded - so widgets can play with
 * real embeddings without paying for training one live.
 */
case class EmbeddingModel(vectors: Map[String, Array[Double]]) {

  def contains(word: String): Boolean = vectors.contains(word.trim.toLowerCase)

  def vector(word: String): Option[Array[Double]] = vectors.get(word.trim.toLowerCase)

  def cosine(a: Array[Double], b: Array[Double]): Double =
    var dot = 0.0; var na = 0.0; var nb = 0.0
    var i = 0
    while i < a.length do
      dot += a(i) * b(i); na += a(i) * a(i); nb += b(i) * b(i)
      i += 1
    if na == 0.0 || nb == 0.0 then 0.0 else dot / (math.sqrt(na) * math.sqrt(nb))

  /** The `n` words whose vectors are most cosine-similar to `target`, excluding any word in `exclude`. */
  def nearest(target: Array[Double], n: Int = 8, exclude: Set[String] = Set.empty): Vector[(String, Double)] =
    vectors.iterator
      .filterNot((w, _) => exclude.contains(w))
      .map((w, v) => (w, cosine(target, v)))
      .toVector
      .sortBy(-_._2)
      .take(n)

  /** Vector arithmetic on words: sums the `positive` words' vectors and subtracts the `negative` ones,
    * then looks up the nearest real words to the result - the classic "king - man + woman" analogy trick. */
  def analogy(positive: Seq[String], negative: Seq[String], n: Int = 5): Option[Vector[(String, Double)]] =
    val pos = positive.map(w => vector(w))
    val neg = negative.map(w => vector(w))
    if pos.exists(_.isEmpty) || neg.exists(_.isEmpty) then None
    else
      val dims = vectors.head._2.length
      val result = Array.fill(dims)(0.0)
      for v <- pos.flatten do for i <- 0 until dims do result(i) += v(i)
      for v <- neg.flatten do for i <- 0 until dims do result(i) -= v(i)
      val used = (positive ++ negative).map(_.trim.toLowerCase).toSet
      Some(nearest(result, n, exclude = used))
}

object EmbeddingModel {

  val assetUrl = "data/glove50d.txt"

  /** Parses the "word v1 v2 ... v50" text format (one word per line, space-separated). */
  def parse(text: String): EmbeddingModel =
    val vectors = text.linesIterator.flatMap { line =>
      val parts = line.split(' ')
      if parts.length < 2 then None
      else Some(parts(0) -> parts.tail.map(_.toDouble))
    }.toMap
    EmbeddingModel(vectors)

  // Cached so every widget on every slide that wants the vectors shares one fetch, rather than
  // each widget re-downloading the same ~4MB file.
  private var cached: Option[Future[EmbeddingModel]] = None

  def load(): Future[EmbeddingModel] = cached match
    case Some(f) => f
    case None =>
      val f = for
        r <- dom.fetch(assetUrl)
        text <- r.text(): Future[String]
      yield parse(text)
      cached = Some(f)
      f
}
