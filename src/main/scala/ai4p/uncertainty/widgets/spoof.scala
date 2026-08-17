package ai4p.uncertainty.widgets

import com.wbillingsley.veautiful.html.{<, Styling, DHtmlComponent, VHtmlContent, ^}
import scala.util.Random

import ai4p.{*, given}
import Common.*

import site.given

/**
 * The pub game "Spoof": each player secretly hides 0–3 coins in a closed fist, then everyone
 * tries to guess the *total* number of coins across all hands. There's no way to know for certain
 * what anyone else is holding — but a regular opponent often has *tendencies*: maybe they rarely
 * repeat their last number, or tend to alternate between a low and a high count. Modelling that as
 * a Markov chain (a transition probability for "what they'll play next, given what they just
 * played") is enough to make a genuinely better-than-random guess at the total.
 */
object Spoof {

  val maxCoins = 4 // coin counts run 0..maxCoins-1, i.e. 0 to 3 coins

  case class Personality(name: String, transition: Vector[Vector[Double]])

  private def normalizeRows(rows: Vector[Vector[Double]]): Vector[Vector[Double]] =
    rows.map(r => r.map(_ / r.sum))

  /** Tends to stick close to whatever they last played. */
  val alex: Personality = Personality("Alex", normalizeRows(Vector(
    Vector(3, 2, 1, 1),
    Vector(1, 3, 2, 1),
    Vector(1, 2, 3, 1),
    Vector(1, 1, 2, 3)
  )))

  /** Tends to alternate between the opposite ends of the range. */
  val sam: Personality = Personality("Sam", normalizeRows(Vector(
    Vector(1, 1, 1, 5),
    Vector(1, 1, 3, 3),
    Vector(3, 3, 1, 1),
    Vector(5, 1, 1, 1)
  )))

  val opponents: Vector[Personality] = Vector(alex, sam)

  def convolve(a: Vector[Double], b: Vector[Double]): Vector[Double] =
    val out = Array.fill(a.size + b.size - 1)(0.0)
    for i <- a.indices; j <- b.indices do out(i + j) += a(i) * b(j)
    out.toVector

  val styling = Styling(
    """|display: inline-block;
       |font-family: 'Lato', sans-serif;
       |max-width: 820px;
       |""".stripMargin
  ).modifiedBy(
    " .sp-row" -> "display: flex; gap: 24px; align-items: flex-start; flex-wrap: wrap; margin: 10px 0;",
    " .sp-panel" -> "min-width: 220px;",
    " .sp-title" -> "font-size: 0.85rem; font-weight: bold; color: #555; margin-bottom: 4px;",
    " .sp-coin-btn" -> "display: inline-flex; align-items: center; justify-content: center; width: 34px; height: 34px; border-radius: 50%; border: 2px solid #ccc; background: white; cursor: pointer; margin-right: 6px; font-weight: bold;",
    " .sp-coin-btn.sp-chosen" -> "border-color: #3b82f6; background: #eff6ff; color: #1d4ed8;",
    " .sp-state" -> "font-size: 0.85rem; color: #444; margin-bottom: 4px;",
    " .sp-bar-row" -> "display: flex; align-items: center; gap: 6px; margin: 2px 0; font-size: 0.8rem;",
    " .sp-bar-label" -> "width: 18px; text-align: right; color: #666;",
    " .sp-bar-track" -> "flex: 1; height: 10px; background: #eee; border-radius: 3px; overflow: hidden;",
    " .sp-bar-fill" -> "height: 100%; background: #a855f7;",
    " .sp-bar-fill.sp-total" -> "background: #16a34a;",
    " .sp-bar-fill.sp-guess" -> "background: #16a34a; outline: 2px solid #166534;",
    " .sp-bar-pct" -> "width: 34px; font-size: 0.75rem; color: #666;",
    " .sp-reveal" -> "margin-top: 10px; padding: 8px; background: #f8f8f8; border-radius: 4px; font-size: 0.9rem;",
    " .sp-reveal.sp-win" -> "background: #dcfce7;",
    " .sp-reveal.sp-lose" -> "background: #fee2e2;",
    " .sp-history" -> "margin-top: 8px; font-size: 0.8rem; color: #666;",
    " .sp-controls" -> "margin-top: 10px; display: flex; gap: 8px; align-items: center; flex-wrap: wrap;"
  ).register()
}

case class SpoofWidget(rng: Random = new Random()) extends DHtmlComponent {
  import Spoof._

  var yourCoins: Int = 1
  var opponentStates: Vector[Int] = opponents.map(_ => rng.nextInt(maxCoins))
  var lastGuess: Int = 0
  var lastReveal: Option[(Vector[Int], Int, Boolean)] = None // (opponents' actual plays, total, guess was right)
  var history: Vector[(Vector[Int], Int, Boolean)] = Vector.empty

  def predictedDist(p: Personality, state: Int): Vector[Double] = p.transition(state)

  def totalDistribution(): Vector[Double] =
    val yours: Vector[Double] = Vector.tabulate(maxCoins)(i => if i == yourCoins then 1.0 else 0.0)
    opponents.zip(opponentStates).foldLeft(yours) { case (acc, (p, s)) => convolve(acc, predictedDist(p, s)) }

  def smartGuess(dist: Vector[Double]): Int = dist.zipWithIndex.maxBy(_._1)._2

  def setYourCoins(c: Int): Unit =
    yourCoins = c
    rerender()

  def reveal(): Unit =
    val dist = totalDistribution()
    val guess = smartGuess(dist)
    val played = opponents.zip(opponentStates).map((p, s) => sampleIndex(p.transition(s), rng))
    val total = yourCoins + played.sum
    val result = (played, total, guess == total)
    lastGuess = guess
    lastReveal = Some(result)
    history = (history :+ result).takeRight(6)
    opponentStates = played
    rerender()

  private def sampleIndex(probs: Vector[Double], rng: Random): Int =
    val r = rng.nextDouble()
    var cum = 0.0
    var i = 0
    while i < probs.size - 1 && { cum += probs(i); r > cum } do i += 1
    i

  def newRound(): Unit =
    lastReveal = None
    rerender()

  private def barChart(dist: Vector[Double], highlight: Int => Boolean, extraCls: String): VHtmlContent =
    <.div(
      for (p, i) <- dist.zipWithIndex yield
        <.div(^.cls := "sp-bar-row",
          <.span(^.cls := "sp-bar-label", i.toString),
          <.div(^.cls := "sp-bar-track",
            <.div(^.cls := s"sp-bar-fill $extraCls${if highlight(i) then " sp-guess" else ""}", ^.style := s"width: ${(p * 100).round}%;")
          ),
          <.span(^.cls := "sp-bar-pct", s"${(p * 100).round}%")
        )
    )

  override protected def render =
    val dist = totalDistribution()
    val guess = smartGuess(dist)

    <.div(^.cls := styling.className,
      <.p(^.cls := "br-label", "You're playing against two regulars. Pick your coins, then see the model's best guess at the total before you Reveal."),

      <.div(^.cls := "sp-panel",
        <.div(^.cls := "sp-title", "Your coins"),
        for c <- 0 until maxCoins yield
          <.span(^.cls := s"sp-coin-btn${if c == yourCoins then " sp-chosen" else ""}", ^.onClick --> setYourCoins(c), c.toString)
      ),

      <.div(^.cls := "sp-row",
        for (p, i) <- opponents.zipWithIndex yield
          <.div(^.cls := "sp-panel",
            <.div(^.cls := "sp-title", s"${p.name} — predicted next play"),
            <.div(^.cls := "sp-state", s"last played: ${opponentStates(i)}"),
            barChart(predictedDist(p, opponentStates(i)), _ => false, "")
          )
      ),

      <.div(^.cls := "sp-panel",
        <.div(^.cls := "sp-title", "Predicted total (you + both opponents)"),
        barChart(dist, _ == guess, "sp-total")
      ),

      {
        lastReveal match
          case Some((played, total, won)) =>
            <.div(^.cls := s"sp-reveal ${if won then "sp-win" else "sp-lose"}",
              s"Actual plays — ${opponents.zip(played).map((p, c) => s"${p.name}: $c").mkString(", ")}. " +
                s"Total was $total (you guessed $lastGuess) — ${if won then "the model's guess was right!" else "the model's guess was wrong this time."}"
            )
          case None => <.span()
      },

      if history.nonEmpty then
        <.div(^.cls := "sp-history",
          "Recent totals: " + history.map((_, total, won) => s"$total${if won then "✓" else "✗"}").mkString(", ")
        )
      else <.span(),

      <.div(^.cls := "sp-controls",
        if lastReveal.isEmpty then
          <.button(^.cls := "btn btn-primary btn-sm", ^.onClick --> reveal(), "Reveal")
        else
          <.button(^.cls := "btn btn-outline-primary btn-sm", ^.onClick --> newRound(), "Next round")
      )
    )
}
