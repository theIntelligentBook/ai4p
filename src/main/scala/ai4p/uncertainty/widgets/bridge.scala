package ai4p.uncertainty.widgets

import scala.util.Random

enum Suit(val char:Char, val colour:String):
  case Clubs extends Suit('♣', "#222")
  case Diamonds extends Suit('♦', "#a44")
  case Hearts extends Suit('♥', "#a44")
  case Spades extends Suit('♠', "#222")

object Suit {
  /** Standard bridge hand-diagram order: Spades, Hearts, Diamonds, Clubs. */
  val displayOrder: Vector[Suit] = Vector(Suit.Spades, Suit.Hearts, Suit.Diamonds, Suit.Clubs)
}

type Hand = Seq[Card]

object UnknownCard {
    def prettyString = "??"
    val colour = "#888"
}

type PerceivedHand = Seq[Card | UnknownCard.type]

case class Card(num:Int, suit:Suit) {
  def prettyString = num match {
    case 11 => s"J${suit.char}"
    case 12 => s"Q${suit.char}"
    case 13 => s"K${suit.char}"
    case 14 => s"A${suit.char}"
    case n => s"$n${suit.char}"
  }
}

object Card {

    def unshuffledDeck = {
        for
            s <- Suit.values.toSeq
            i <- 2 to 14
        yield Card(i, s)
    }

    /** Sorts a hand into standard bridge hand-diagram order: by suit (S,H,D,C), then rank high to low. */
    def sortHand(hand: Hand): Hand =
      hand.sortBy(c => (Suit.displayOrder.indexOf(c.suit), -c.num))

    private def rankOf(c: Char): Int = c.toUpper match
      case 'A' => 14
      case 'K' => 13
      case 'Q' => 12
      case 'J' => 11
      case 'T' => 10
      case d if d.isDigit => d.asDigit
      case other => throw new IllegalArgumentException(s"Unrecognised card rank '$other'")

    /**
     * Parses a hand written in standard bridge shorthand: one group of ranks per suit, in
     * Spades.Hearts.Diamonds.Clubs order, separated by dots — e.g. `"AKQJT.-.-.-"` is the
     * ace-king-queen-jack-ten of spades and nothing else. Use `"-"` for a suit the hand is void in.
     */
    def parseHand(spec: String): Hand =
      val groups = spec.split("\\.", -1)
      require(groups.length == Suit.displayOrder.length, s"Expected ${Suit.displayOrder.length} dot-separated suits, got '$spec'")
      Suit.displayOrder.zip(groups).flatMap { (suit, ranks) =>
        if ranks == "-" then Seq.empty else ranks.map(c => Card(rankOf(c), suit))
      }
}

enum Seat:
  case North, East, South, West

  /** The seat immediately clockwise from this one (the order play rotates in). */
  def next: Seat = Seat.fromOrdinal((ordinal + 1) % 4)

  /** This seat's partner, sitting opposite. */
  def partner: Seat = Seat.fromOrdinal((ordinal + 2) % 4)

  /** True for North/South, false for East/West — the two bridge "sides". */
  def isNS: Boolean = this == Seat.North || this == Seat.South

object Seat {
  val all: Vector[Seat] = Vector(North, East, South, West)
}

/** The four hands in a deal. */
case class Deal(hands: Map[Seat, Hand]) {
  def handOf(seat: Seat): Hand = hands.getOrElse(seat, Seq.empty)

  def withCardRemoved(seat: Seat, card: Card): Deal =
    copy(hands = hands.updated(seat, handOf(seat).filterNot(_ == card)))
}

object Deal {
  /**
   * Deals a small random "endgame" position: `cardsPerHand` cards to each seat, drawn only from
   * the top `cardsPerHand` ranks of each suit (e.g. 5 cards per hand uses 10 through Ace). This is
   * a simplification, not the result of playing a full 13-card deal down to an ending — but it
   * keeps hands small enough that an exhaustive minimax search stays fast in a browser widget,
   * without the odd gappy-looking hands a random subset of the full deck would produce.
   */
  def random(cardsPerHand: Int = 5, rng: Random = new Random()): Deal =
    custom(cardsPerHand, Map.empty, rng)

  /**
   * Like [[random]], but any seat named in `fixedHands` gets the exact hand given (parsed with
   * [[Card.parseHand]]) instead of a random one — handy for setting up a specific teaching
   * position (e.g. "West is void in spades") while leaving the other seats' hands random. The
   * remaining seats are dealt randomly from whatever's left of the top `cardsPerHand` ranks.
   */
  def custom(cardsPerHand: Int = 5, fixedHands: Map[Seat, String], rng: Random = new Random()): Deal =
    val fixed = fixedHands.map((seat, spec) => seat -> Card.parseHand(spec))
    val usedCards = fixed.values.flatten.toSet
    val minRank = 15 - cardsPerHand
    val pool = rng.shuffle(Card.unshuffledDeck.filter(c => c.num >= minRank && !usedCards.contains(c)))
    val remainingSeats = Seat.all.filterNot(fixed.contains)
    val dealt = remainingSeats.zipWithIndex.map { (seat, i) =>
      seat -> pool.slice(i * cardsPerHand, (i + 1) * cardsPerHand)
    }.toMap
    Deal(fixed ++ dealt)
}

/** One completed trick: the cards played, in turn order starting with the leader. */
case class Trick(plays: Vector[(Seat, Card)]) {
  def ledSuit: Suit = plays.head._2.suit

  /** The seat that wins this trick, given the trump suit (if any). */
  def winner(trump: Option[Suit]): Seat =
    val led = ledSuit
    def rank(c: Card): Int =
      if trump.contains(c.suit) then c.num + 100
      else if c.suit == led then c.num
      else -1
    plays.maxBy((_, c) => rank(c))._1
}

/**
 * The state of a hand in progress: what's left in each hand, the trump suit, whose turn it is to
 * play, the (possibly partial) current trick, and the history of completed tricks.
 */
case class PlayState(
  deal: Deal,
  trump: Option[Suit],
  toPlay: Seat,
  currentTrick: Vector[(Seat, Card)] = Vector.empty,
  history: Vector[Trick] = Vector.empty
) {

  def ledSuit: Option[Suit] = currentTrick.headOption.map(_._2.suit)

  /** Legal cards `seat` may play right now: must follow suit if able to. */
  def legalPlays(seat: Seat): Hand =
    val hand = deal.handOf(seat)
    ledSuit match
      case Some(suit) if hand.exists(_.suit == suit) => hand.filter(_.suit == suit)
      case _ => hand

  /** Plays a card for the current seat to move, resolving the trick (and advancing the lead to its winner) if this completes it. */
  def play(card: Card): PlayState =
    val seat = toPlay
    val newDeal = deal.withCardRemoved(seat, card)
    val newTrick = currentTrick :+ (seat -> card)
    if newTrick.size == 4 then
      val trick = Trick(newTrick)
      copy(deal = newDeal, toPlay = trick.winner(trump), currentTrick = Vector.empty, history = history :+ trick)
    else
      copy(deal = newDeal, toPlay = seat.next, currentTrick = newTrick)

  /** Tricks won so far, (North/South, East/West). */
  def tricksWon: (Int, Int) =
    val ns = history.count(_.winner(trump).isNS)
    (ns, history.size - ns)

  /** Suits each seat is known to be void in, inferred from failing to follow suit in a past trick. */
  def knownVoids: Map[Seat, Set[Suit]] =
    history.foldLeft(Map.empty[Seat, Set[Suit]]) { (voids, trick) =>
      val led = trick.ledSuit
      trick.plays.foldLeft(voids) { case (v, (seat, card)) =>
        if card.suit != led then v.updated(seat, v.getOrElse(seat, Set.empty) + led) else v
      }
    }
}
