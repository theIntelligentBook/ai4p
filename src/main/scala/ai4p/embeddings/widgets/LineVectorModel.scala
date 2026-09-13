package ai4p.embeddings.widgets

/** The "which lines does this word appear in" vectors, built over the same [[peterRabbit]] text
  * as [[PeterRabbitModel]] - the simplest possible bag-of-words / co-occurrence representation.
  * Pure data/maths, no UI. */
object LineVectorModel {

  val corpus: Vector[String] = PeterRabbitModel.lines

  /** word -> one bit per line, 1 if the word appears in that line - reusing [[PeterRabbitModel]]'s
    * parsing so both widgets agree on what a "word" and a "line" are. */
  val vectors: Map[String, Vector[Int]] = PeterRabbitModel.vectors

  /** Common English function words - articles, pronouns, prepositions, auxiliary verbs, and the
    * like. They show up in almost every paragraph regardless of what it's about, so leaving them
    * in would swamp the grid (and every "most related" list) with words that aren't really *about*
    * anything. Keys are letters-only/lower-case, matching [[PeterRabbitModel.keyOf]], so a
    * contraction like "don't" is listed as "dont". */
  private val stopwords: Set[String] = Set(
    "i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "youre", "youve", "youll",
    "youd", "your", "yours", "yourself", "yourselves", "he", "him", "his", "himself", "she", "shes",
    "her", "hers", "herself", "it", "its", "itself", "they", "them", "their", "theirs",
    "themselves", "what", "which", "who", "whom", "this", "that", "thatll", "these", "those", "am",
    "is", "are", "was", "were", "be", "been", "being", "have", "has", "had", "having", "do", "does",
    "did", "doing", "a", "an", "the", "and", "but", "if", "or", "because", "as", "until", "while",
    "of", "at", "by", "for", "with", "about", "against", "between", "into", "through", "during",
    "before", "after", "above", "below", "to", "from", "up", "down", "in", "out", "on", "off",
    "over", "under", "again", "further", "then", "once", "here", "there", "when", "where", "why",
    "how", "all", "any", "both", "each", "few", "more", "most", "other", "some", "such", "no",
    "nor", "not", "only", "own", "same", "so", "than", "too", "very", "s", "t", "can", "will",
    "just", "dont", "should", "shouldve", "now", "d", "ll", "m", "o", "re", "ve", "y", "aint",
    "arent", "couldn", "couldnt", "didnt", "doesnt", "hadnt", "hasnt", "havent", "isnt", "ma",
    "mightnt", "mustnt", "neednt", "shant", "shouldnt", "wasnt", "werent", "wont", "wouldnt",
    "upon", "one"
  )

  /** Every word in the story that isn't a stopword, most-mentioned first. */
  val vocabulary: Vector[String] =
    vectors.keys.toVector
      .filterNot(stopwords.contains)
      .sortBy(w => (-vectors(w).sum, w))

  /** A small, legible spread of the story's words - frequent, rare, and thematically paired
    * (peter/mcgregor, cat/mouse, garden/gate, ...) - for widgets that plot or list words but can't
    * show the whole vocabulary at once. */
  val curatedSample: Vector[String] = Vector(
    "peter", "mcgregor", "rabbit", "mother",
    "garden", "gate", "wood",
    "cat", "mouse", "sparrows",
    "cabbages", "lettuces", "parsley", "radishes",
    "jacket", "shoes", "wheelbarrow", "bread"
  ).filter(vectors.contains)

  def cosine(a: Vector[Int], b: Vector[Int]): Double =
    val dot = a.zip(b).map(_ * _).sum
    val na = math.sqrt(a.map(x => x * x).sum.toDouble)
    val nb = math.sqrt(b.map(x => x * x).sum.toDouble)
    if na == 0.0 || nb == 0.0 then 0.0 else dot / (na * nb)
}
