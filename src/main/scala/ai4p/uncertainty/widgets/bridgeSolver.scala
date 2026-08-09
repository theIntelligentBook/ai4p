package ai4p.uncertainty.widgets

import scala.util.Random
import scala.collection.mutable

/**
 * An exact ("double-dummy") solver for small, fully-known bridge endings: plain alpha-beta
 * minimax with memoization on the remaining-cards state. That's plenty fast for the handful of
 * cards per hand this teaching widget deals out, but it's nowhere near what a real double-dummy
 * solver needs to stay fast on full 13-card deals — those rely on much more specialised
 * techniques than this file attempts.
 */
object BridgeSolver {

  /**
   * A cache key covering only what actually determines the remaining game's value: what's left
   * in each hand, whose turn it is, and the in-progress trick — not the history of how we got
   * there. Keying on the full `PlayState` (which also carries `history`) would mean two different
   * move orders that reach the *identical* remaining position never share a cache hit — exactly
   * the case that matters most, e.g. a hand with several interchangeable cards to choose from.
   */
  private case class SolveKey(deal: Deal, toPlay: Seat, currentTrick: Vector[(Seat, Card)])
  private def keyOf(state: PlayState): SolveKey = SolveKey(state.deal, state.toPlay, state.currentTrick)

  /** True if seat `after` just won a trick in the transition from `before` to `after`. */
  private def trickJustWonBy(before: PlayState, after: PlayState): Option[Seat] =
    if after.history.size == before.history.size + 1 then Some(after.history.last.winner(before.trump))
    else None

  /**
   * Reduces `moves` to one representative per group of cards that are provably interchangeable
   * right now: same suit, with no card held by the *opposing side* sitting at a rank strictly
   * between them. Playing any card in such a "touching" group leads to exactly the same optimal
   * value, so there's no need to search more than one of them — this is the standard "equivalent
   * card" reduction real double-dummy solvers rely on. Without it, a hand holding an entire
   * uncontested suit (or all the remaining trumps) forces the search to try every permutation of
   * cards that are actually tied, which blows the tree up combinatorially.
   */
  private def reduceEquivalentMoves(state: PlayState, seat: Seat, moves: Hand): Hand =
    moves.groupBy(_.suit).values.flatMap { cardsInSuit =>
      val suit = cardsInSuit.head.suit
      val opponentRanks = state.deal.hands.iterator
        .collect { case (s, hand) if s.isNS != seat.isNS => hand }
        .flatten.filter(_.suit == suit).map(_.num).toSet
      val sorted = cardsInSuit.sortBy(-_.num) // highest first
      sorted.foldLeft(Vector.empty[Card]) { (kept, card) =>
        kept.lastOption match
          case Some(top) if !opponentRanks.exists(r => r < top.num && r > card.num) => kept // still tied with the kept representative
          case _ => kept :+ card // starts a new equivalence group
      }
    }.toVector

  private def minimax(state: PlayState, alphaIn: Int, betaIn: Int, cache: mutable.HashMap[SolveKey, Int]): Int =
    // Returns North/South's tricks won from `state` onward, with both sides playing optimally.
    if state.deal.handOf(state.toPlay).isEmpty then 0
    else cache.getOrElseUpdate(keyOf(state), {
      val seat = state.toPlay
      val maximizing = seat.isNS
      var alpha = alphaIn
      var beta = betaIn
      var best = if maximizing then Int.MinValue else Int.MaxValue
      val moves = reduceEquivalentMoves(state, seat, state.legalPlays(seat)).iterator
      var pruned = false
      while moves.hasNext && !pruned do
        val card = moves.next()
        val next = state.play(card)
        val wonThisTrick = if trickJustWonBy(state, next).exists(_.isNS) then 1 else 0
        val value = wonThisTrick + minimax(next, alpha, beta, cache)
        if maximizing then
          if value > best then best = value
          if best > alpha then alpha = best
        else
          if value < best then best = value
          if best < beta then beta = best
        if beta <= alpha then pruned = true
      best
    })

  /** The exact number of the remaining tricks (from `state` onward) North/South can force with optimal play by both sides. */
  def nsTricksFromHere(state: PlayState): Int =
    minimax(state, Int.MinValue, Int.MaxValue, mutable.HashMap.empty)

  /**
   * Ranks `state.toPlay`'s legal cards by the resulting double-dummy outcome for *their own
   * side* — higher is always better for the mover, whichever side they're on. Assumes every hand
   * in `state` is fully known. Every actual legal card gets its own entry here (the equivalent-card
   * reduction is only applied to the recursive search *below* this point, not to what's reported),
   * so callers — including the Monte Carlo bot's per-card score averaging — always get a complete,
   * per-card ranking.
   */
  def rankMoves(state: PlayState): Vector[(Card, Int)] =
    val seat = state.toPlay
    val totalRemaining = state.deal.handOf(seat).size
    val cache = mutable.HashMap.empty[SolveKey, Int]
    state.legalPlays(seat).map { card =>
      val next = state.play(card)
      val wonThisTrick = if trickJustWonBy(state, next).exists(_.isNS) then 1 else 0
      val nsFuture = wonThisTrick + minimax(next, Int.MinValue, Int.MaxValue, cache)
      val mySideFuture = if seat.isNS then nsFuture else totalRemaining - nsFuture
      card -> mySideFuture
    }.sortBy((_, v) => -v).toVector
}

/**
 * A Monte Carlo "determinization" bridge bot: it can't see the hidden seats' hands, so it
 * repeatedly guesses ("samples") a full, legally-consistent deal — respecting how many cards
 * each hidden seat holds and any suits they've already shown void in — solves each guess exactly
 * with [[BridgeSolver]], and averages the results to rank the mover's candidate cards. This is
 * essentially how real-world Monte Carlo bridge-playing programs choose a card.
 */
object MonteCarloBridgeBot {

  /**
   * Randomized backtracking partition of `cards` into groups of the sizes given in `needs`,
   * never giving a seat a card of a suit it's already known to be void in.
   */
  private def partitionRespectingVoids(
    cards: Vector[Card],
    needs: Map[Seat, Int],
    voids: Map[Seat, Set[Suit]],
    rng: Random
  ): Option[Map[Seat, Hand]] =
    def go(remaining: List[Card], needsLeft: Map[Seat, Int], acc: Map[Seat, Vector[Card]]): Option[Map[Seat, Vector[Card]]] =
      remaining match
        case Nil => Some(acc)
        case card :: rest =>
          val candidateSeats = rng.shuffle(needsLeft.iterator.collect {
            case (seat, n) if n > 0 && !voids.getOrElse(seat, Set.empty).contains(card.suit) => seat
          }.toVector)
          candidateSeats.iterator
            .map(seat => go(rest, needsLeft.updated(seat, needsLeft(seat) - 1), acc.updated(seat, acc(seat) :+ card)))
            .find(_.isDefined)
            .flatten
    go(cards.toList, needs, needs.map((s, _) => s -> Vector.empty[Card]))

  /** Randomly re-deals the cards currently held by `hiddenSeats`, respecting hand sizes and known voids. */
  def sampleDeal(state: PlayState, hiddenSeats: Set[Seat], rng: Random): Option[Deal] =
    val voids = state.knownVoids
    val pool = hiddenSeats.toVector.flatMap(state.deal.handOf)
    val needs = hiddenSeats.map(s => s -> state.deal.handOf(s).size).toMap
    partitionRespectingVoids(rng.shuffle(pool), needs, voids, rng).map(assignment => Deal(state.deal.hands ++ assignment))

  /**
   * Estimates the best move for `state.toPlay`, from the point of view of a player who can only
   * see `visibleSeats` (plus every card already played). Averages an exact double-dummy solve
   * over `samples` randomly-sampled hidden deals. Returns the mover's legal cards ranked best
   * first, each with its average score (tricks for the mover's own side).
   */
  def rankMoves(
    state: PlayState,
    visibleSeats: Set[Seat],
    samples: Int = 50,
    rng: Random = new Random()
  ): Vector[(Card, Double)] =
    val hiddenSeats = Seat.all.filterNot(visibleSeats.contains).toSet
    if hiddenSeats.isEmpty then
      BridgeSolver.rankMoves(state).map((card, v) => card -> v.toDouble)
    else
      val totals = mutable.Map.empty[Card, Double].withDefaultValue(0.0)
      val counts = mutable.Map.empty[Card, Int].withDefaultValue(0)
      var attempts = 0
      var solved = 0
      while solved < samples && attempts < samples * 4 do
        attempts += 1
        sampleDeal(state, hiddenSeats, rng).foreach { sampledDeal =>
          val sampledState = state.copy(deal = sampledDeal)
          for (card, score) <- BridgeSolver.rankMoves(sampledState) do
            totals(card) += score
            counts(card) += 1
          solved += 1
        }
      state.legalPlays(state.toPlay)
        .map(card => card -> (if counts(card) > 0 then totals(card) / counts(card) else 0.0))
        .sortBy((_, v) => -v)
        .toVector

  /**
   * Estimates, for every card still unseen in `hiddenSeats`' hands, the probability of it
   * currently being in each of those seats — by the same random-sampling approach as
   * [[rankMoves]], but just tallying where cards land rather than solving anything.
   */
  def cardLocationProbabilities(
    state: PlayState,
    hiddenSeats: Set[Seat],
    samples: Int = 200,
    rng: Random = new Random()
  ): Map[Card, Map[Seat, Double]] =
    val voids = state.knownVoids
    val pool = hiddenSeats.toVector.flatMap(state.deal.handOf)
    val needs = hiddenSeats.map(s => s -> state.deal.handOf(s).size).toMap
    val drawnSamples = Iterator.continually(partitionRespectingVoids(rng.shuffle(pool), needs, voids, rng))
      .flatten
      .take(samples)
      .toVector
    val total = math.max(1, drawnSamples.size)
    pool.map { card =>
      val bySeat = hiddenSeats.map { seat =>
        seat -> drawnSamples.count(assignment => assignment(seat).contains(card)).toDouble / total
      }.toMap
      card -> bySeat
    }.toMap
}
