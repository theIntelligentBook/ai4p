package ai4p.uncertainty.widgets

import com.wbillingsley.veautiful.html.{<, Styling, ^, VHtmlContent}

import ai4p.{given, *}

/** Rendering helpers and shared styling for the bridge widgets. */
object BridgeCommon {

  val styling = Styling(
    """|display: inline-block;
       |font-family: 'Lato', sans-serif;
       |max-width: 1200px;
       |""".stripMargin
  ).modifiedBy(
    " .br-table-layout" -> "display: flex; gap: 16px; align-items: flex-start; flex-wrap: nowrap;",
    " .br-history" -> "flex: 0 0 250px; min-width: 150px; max-height: 280px; overflow-y: auto; border-left: 1px solid #ddd; padding-left: 12px;",
    " .br-history-trick" -> "display: flex; align-items: center; gap: 4px; flex-wrap: wrap; margin: 4px 0; font-size: 0.85rem;",
    " .br-history-winner .br-card" -> "border-color: #16a34a; background: #f0fdf4;",
    " .br-compass" -> "display: grid; grid-template-columns: minmax(250px, 1fr) minmax(250px, 1fr) minmax(250px, 1fr); grid-template-areas: '. north .' 'west center east' '. south .'; gap: 6px; align-items: center; justify-items: center; margin: 10px 0;",
    " .br-compass-north" -> "grid-area: north;",
    " .br-compass-east" -> "grid-area: east;",
    " .br-compass-south" -> "grid-area: south;",
    " .br-compass-west" -> "grid-area: west;",
    " .br-compass-center" -> "grid-area: center; display: flex; align-items: center; justify-content: center;",
    " .br-hand-row" -> "display: flex; flex-direction: column; align-items: center; gap: 2px; margin: 3px 0;",
    " .br-hand-cards" -> "display: flex; align-items: center; gap: 4px; flex-wrap: wrap; justify-content: center;",
    " .br-seat-label" -> "font-weight: bold; color: #444; font-size: 0.75rem; text-transform: uppercase; letter-spacing: 0.03em;",
    " .br-seat-label.br-turn" -> "color: #b91c1c;",
    " .br-card" -> "display: inline-block; padding: 2px 6px; border: 1px solid #ccc; border-radius: 4px; background: white; font-family: monospace; font-size: 0.95rem;",
    " .br-card.br-playable" -> "cursor: pointer; border-color: #3b82f6; background: #eff6ff;",
    " .br-card.br-playable:hover" -> "background: #dbeafe;",
    " .br-card.br-unknown" -> "color: #999; background: #f3f4f6;",
    " .br-void-note" -> "font-size: 0.75rem; color: #888; margin-left: 4px;",
    " .br-trick-row" -> "display: flex; gap: 10px; align-items: center; margin: 8px 0; padding: 6px; border-radius: 4px; min-height: 28px; text-align: center;",
    " .br-score" -> "font-size: 0.9rem; color: #333; margin: 4px 0;",
    " .br-controls" -> "margin-top: 8px; display: flex; gap: 8px; align-items: center; flex-wrap: wrap;",
    " .br-section-title" -> "font-size: 0.85rem; font-weight: bold; color: #555; margin-top: 10px;",
    " .br-moves" -> "margin-top: 4px;",
    " .br-move-btn" -> "display: block; width: 100%; text-align: left; margin: 2px 0; padding: 3px 8px; border-radius: 4px; border: 1px solid #ddd; background: white; cursor: pointer; font-family: monospace;",
    " .br-move-btn:hover" -> "background: #f0f9ff;",
    " .br-probs" -> "margin-top: 4px; font-size: 0.85rem;",
    " .br-prob-row" -> "display: flex; align-items: center; gap: 6px; margin: 2px 0;",
    " .br-prob-bar" -> "flex: 1; display: flex; height: 12px; border-radius: 3px; overflow: hidden; background: #eee;",
    " .br-label" -> "font-size: 0.75rem; color: #666; white-space: nowrap;"
  ).register()

  def seatColour(seat: Seat): String = seat match
    case Seat.North => "#3b82f6"
    case Seat.East  => "#f59e0b"
    case Seat.South => "#16a34a"
    case Seat.West  => "#a855f7"

  def cardBadge(card: Card, playable: Boolean, action: => Unit): VHtmlContent =
    <.span(
      ^.cls := s"br-card${if playable then " br-playable" else ""}",
      ^.style := s"color: ${card.suit.colour};",
      ^.onClick --> (if playable then action else ()),
      card.prettyString
    )

  def unknownBadge(): VHtmlContent =
    <.span(^.cls := "br-card br-unknown", UnknownCard.prettyString)

  /** One seat's label above its hand — real cards if `hand` is given, otherwise `hiddenCount` unknown badges. */
  def handRow(seat: Seat, isTurn: Boolean, hand: Option[Hand], hiddenCount: Int, playable: Card => Boolean, onPlay: Card => Unit): VHtmlContent =
    <.div(^.cls := "br-hand-row",
      <.span(^.cls := s"br-seat-label${if isTurn then " br-turn" else ""}", seat.toString),
      <.div(^.cls := "br-hand-cards",
        hand match
          case Some(h) => for c <- Card.sortHand(h) yield cardBadge(c, playable(c), onPlay(c))
          case None => for _ <- 0 until hiddenCount yield unknownBadge()
      )
    )

  /**
   * Arranges the four seats' content in the standard bridge compass layout — North above, South
   * below, West and East to either side — with `center` (typically the current trick) in the
   * middle of the 3x3 grid.
   */
  def compassLayout(north: VHtmlContent, east: VHtmlContent, south: VHtmlContent, west: VHtmlContent, center: VHtmlContent): VHtmlContent =
    <.div(^.cls := "br-compass",
      <.div(^.cls := "br-compass-north", north),
      <.div(^.cls := "br-compass-east", east),
      <.div(^.cls := "br-compass-south", south),
      <.div(^.cls := "br-compass-west", west),
      <.div(^.cls := "br-compass-center", center)
    )

  /**
   * Places `handsAndTrick` (typically a [[compassLayout]]) next to a scrollable [[trickHistory]]
   * panel. Needed because a completed trick's cards vanish from the "current trick" display the
   * moment the trick resolves — most noticeably when a hidden AI seat plays the last card of a
   * trick, which the viewer would otherwise never see at all — so the history panel is the only
   * place a just-finished trick stays visible.
   */
  def tableLayout(handsAndTrick: VHtmlContent, history: VHtmlContent): VHtmlContent =
    <.div(^.cls := "br-table-layout", handsAndTrick, history)

  /** A scrollable list of completed tricks, each showing every seat's card and who won it. */
  def trickHistory(history: Vector[Trick], trump: Option[Suit]): VHtmlContent =
    <.div(^.cls := "br-history",
      <.div(^.cls := "br-section-title", "Tricks so far"),
      if history.isEmpty then <.p(^.cls := "br-label", "(none yet)")
      else for (trick, i) <- history.zipWithIndex yield
        val winner = trick.winner(trump)
        <.div(^.cls := "br-history-trick",
          <.span(^.cls := "br-label", s"${i + 1}."),
          for (seat, card) <- trick.plays yield
            <.span(^.cls := (if seat == winner then "br-history-winner" else ""),
              <.span(^.cls := "br-label", s"${seat.toString.take(1)}:"),
              cardBadge(card, playable = false, action = ())
            ),
          <.span(^.cls := "br-label", s"($winner won)")
        )
    )

  def trickRow(currentTrick: Vector[(Seat, Card)]): VHtmlContent =
    <.div(^.cls := "br-trick-row",
      <.div(
        <.span(^.cls := "br-label", "Current trick:"), <.br(),
        if currentTrick.isEmpty then <.span(^.cls := "br-label", "(none yet)")
        else for (seat, card) <- currentTrick yield
          <.span(
            <.span(^.cls := "br-label", s"${seat.toString.take(1)}:"),
            cardBadge(card, playable = false, action = ())
          )
      )
    )

  def scoreRow(tricksWon: (Int, Int)): VHtmlContent =
    val (ns, ew) = tricksWon
    <.div(^.cls := "br-score", s"Tricks won — North/South: $ns   East/West: $ew")

  def moveButton(card: Card, label: String, onClick: => Unit): VHtmlContent =
    <.button(^.cls := "br-move-btn", ^.style := s"color: ${card.suit.colour};", ^.onClick --> onClick, label)

  def probRow(card: Card, probs: Map[Seat, Double], hiddenSeats: Seq[Seat]): VHtmlContent =
    <.div(^.cls := "br-prob-row",
      <.span(^.cls := "br-card", ^.style := s"color: ${card.suit.colour};", card.prettyString),
      <.div(^.cls := "br-prob-bar",
        for seat <- hiddenSeats yield
          val pct = (probs.getOrElse(seat, 0.0) * 100).round
          <.div(
            ^.style := s"width: $pct%; background: ${seatColour(seat)};",
            ^.attr("title") := s"$seat: $pct%"
          )
      ),
      <.span(^.cls := "br-label",
        hiddenSeats.map(s => s"$s ${(probs.getOrElse(s, 0.0) * 100).round}%").mkString("   ")
      )
    )
}
