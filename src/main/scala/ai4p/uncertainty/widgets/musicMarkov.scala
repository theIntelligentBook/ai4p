package ai4p.uncertainty.widgets

import com.wbillingsley.veautiful.html.{<, Styling, DHtmlComponent, VHtmlContent, ^}
import org.scalajs.dom
import scala.util.Random

import ai4p.{*, given}
import Common.*

import site.given

/**
 * A little Markov-chain melody generator. The "state" is a musical event — a scale degree plus a
 * note length — and next-event probabilities are produced by a small set of hand-written,
 * explainable rules rather than a giant hardcoded table, tuned to match tendencies that show up
 * again and again in real tonal melodies:
 *
 *   - small melodic steps are far more common than big leaps
 *   - a leading tone (degree 7) pulls strongly towards the tonic (degree 8/1)
 *   - a melody that just leapt tends to "correct" with a step back the other way — the classic
 *     *post-skip reversal* every tonal melody seems to know about (Huron, ''Sweet Anticipation'')
 *   - a couple of common rhythmic pairings (a dotted crotchet is nearly always followed by a
 *     quaver; repeating the same note length is mildly preferred)
 *
 * Switching between a **1st-order** model (context = just the last note) and a **2nd-order**
 * model (context = the last *two* notes) is the whole point of the widget: only the 2nd-order
 * model can "see" that a leap just happened, so only it can apply the post-skip-reversal rule.
 */
object MusicMarkov {

  enum Duration(val beats: Double, val label: String):
    case Quaver extends Duration(0.5, "quaver")
    case Crotchet extends Duration(1.0, "crotchet")
    case DottedCrotchet extends Duration(1.5, "dotted crotchet")
    case Minim extends Duration(2.0, "minim")

  object Duration {
    val all: Vector[Duration] = Vector(Quaver, Crotchet, DottedCrotchet, Minim)
  }
  import Duration._

  enum Scale(val degreeNames: Vector[String], val semitones: Vector[Int]):
    case Major extends Scale(Vector("C", "D", "E", "F", "G", "A", "B", "C"), Vector(0, 2, 4, 5, 7, 9, 11, 12))
    case Minor extends Scale(Vector("C", "D", "Eb", "F", "G", "Ab", "Bb", "C"), Vector(0, 2, 3, 5, 7, 8, 10, 12))

  /** A musical event: a scale degree (1 = tonic, ... 8 = the tonic an octave up) and a note length. */
  case class Note(degree: Int, duration: Duration)

  val degrees: Vector[Int] = (1 to 8).toVector
  val allNotes: Vector[Note] = for d <- degrees; dur <- Duration.all yield Note(d, dur)

  val tonicHz = 261.626 // middle C

  def freqOf(degree: Int, scale: Scale): Double =
    tonicHz * math.pow(2.0, scale.semitones(degree - 1) / 12.0)

  def noteName(note: Note, scale: Scale): String = scale.degreeNames(note.degree - 1)

  private def durationTransitionWeight(from: Duration, to: Duration): Double =
    (from, to) match
      case (DottedCrotchet, Quaver) => 4.0
      case (Quaver, Quaver) => 2.0
      case (Quaver, Crotchet) => 1.5
      case (Minim, Crotchet) => 1.5
      case (a, b) if a == b => 1.5
      case _ => 1.0

  /**
   * The weight of `candidate` as the next event, given `context` (the last one or two events,
   * most recent last). Not normalised — callers divide by the sum over `allNotes`.
   */
  def weight(context: Vector[Note], candidate: Note): Double =
    val last = context.last
    val delta = candidate.degree - last.degree
    val stepWeight = math.exp(-math.abs(delta) * 0.6)
    val leadingToneBonus = if last.degree == 7 && candidate.degree == 8 then 3.0 else 1.0
    val postSkipReversalBonus =
      if context.size >= 2 then
        val prevDelta = context(1).degree - context(0).degree
        val sameDirection = math.signum(prevDelta.toDouble) == math.signum(delta.toDouble)
        if math.abs(prevDelta) >= 3 && delta != 0 && !sameDirection then 2.5 else 1.0
      else 1.0
    stepWeight * leadingToneBonus * postSkipReversalBonus * durationTransitionWeight(last.duration, candidate.duration)

  /** The probability distribution over every possible next event, given `context`. */
  def nextDistribution(context: Vector[Note]): Vector[(Note, Double)] =
    val raw = allNotes.map(cand => cand -> weight(context, cand))
    val total = raw.map(_._2).sum
    raw.map((note, w) => note -> w / total)

  def sampleIndex(probs: Vector[Double], rng: Random): Int =
    val r = rng.nextDouble()
    var cum = 0.0
    var i = 0
    while i < probs.size - 1 && { cum += probs(i); r > cum } do i += 1
    i

  val styling = Styling(
    """|display: inline-block;
       |font-family: 'Lato', sans-serif;
       |max-width: 820px;
       |""".stripMargin
  ).modifiedBy(
    " .mz-sequence" -> "display: flex; gap: 4px; flex-wrap: wrap; margin: 10px 0; min-height: 40px;",
    " .mz-note" -> "display: flex; flex-direction: column; align-items: center; padding: 4px 7px; border: 1px solid #ccc; border-radius: 4px; background: white; min-width: 26px;",
    " .mz-note.mz-current" -> "border-color: #16a34a; background: #f0fdf4;",
    " .mz-note-char" -> "font-weight: bold; font-size: 1rem;",
    " .mz-note-len" -> "font-size: 0.65rem; color: #666;",
    " .mz-state" -> "font-size: 0.9rem; color: #333; margin: 8px 0 4px 0;",
    " .mz-section-title" -> "font-size: 0.85rem; font-weight: bold; color: #555; margin-top: 8px;",
    " .mz-bar-row" -> "display: flex; align-items: center; gap: 6px; margin: 2px 0; font-size: 0.8rem;",
    " .mz-bar-label" -> "width: 92px; color: #666; font-family: monospace;",
    " .mz-bar-track" -> "flex: 1; height: 10px; background: #eee; border-radius: 3px; overflow: hidden; max-width: 220px;",
    " .mz-bar-fill" -> "height: 100%; background: #a855f7;",
    " .mz-bar-pct" -> "width: 34px; font-size: 0.75rem; color: #666;",
    " .mz-controls" -> "margin-top: 10px; display: flex; gap: 14px; align-items: center; flex-wrap: wrap;",
    " .mz-toggle-group" -> "display: inline-flex; gap: 2px;",
    " .mz-toggle" -> "padding: 3px 8px; border: 1px solid #ccc; background: white; cursor: pointer; font-size: 0.8rem; border-radius: 3px;",
    " .mz-toggle.mz-active" -> "background: #3b82f6; border-color: #3b82f6; color: white;",
    " input[type=range]" -> "width: 100px;"
  ).register()
}

import MusicMarkov._

case class MusicMarkovWidget(seed: Note = Note(1, Duration.Crotchet)) extends DHtmlComponent {

  var order: Int = 2
  var scale: Scale = Scale.Major
  var tempoBpm: Int = 108
  var history: Vector[Note] = Vector(seed)
  var playing: Boolean = false
  var audioCtx: Option[dom.AudioContext] = None
  var timerId: Option[Int] = None

  def context(): Vector[Note] = history.takeRight(order)

  def secPerBeat: Double = 60.0 / tempoBpm

  def sampleNext(): Note =
    val dist = nextDistribution(context())
    dist(sampleIndex(dist.map(_._2), Random))._1

  /** Hardcoded attack/release envelope so a note sounds like a key press rather than a harsh on/off click. */
  def playTone(note: Note): Unit =
    audioCtx.foreach { ctx =>
      val osc = ctx.createOscillator()
      val gain = ctx.createGain()
      osc.frequency.value = freqOf(note.degree, scale)
      osc.connect(gain)
      gain.connect(ctx.destination)

      val now = ctx.currentTime
      val dur = note.duration.beats * secPerBeat
      val attack = math.min(0.02, dur * 0.2)
      val release = math.min(0.08, dur * 0.3)
      val sustainEnd = math.max(attack, dur - release)
      val peak = 0.22
      val floor = 0.0001

      gain.gain.setValueAtTime(floor, now)
      gain.gain.exponentialRampToValueAtTime(peak, now + attack)
      gain.gain.setValueAtTime(peak, now + sustainEnd)
      gain.gain.exponentialRampToValueAtTime(floor, now + dur)

      osc.start(now)
      osc.stop(now + dur + 0.02)
    }

  def stepOnce(): Unit =
    val note = sampleNext()
    history = (history :+ note).takeRight(48)
    playTone(note)
    rerender()
    if playing then
      timerId = Some(dom.window.setTimeout(() => stepOnce(), note.duration.beats * secPerBeat * 1000))

  def startPlaying(): Unit =
    if audioCtx.isEmpty then audioCtx = Some(new dom.AudioContext())
    audioCtx.foreach(_.resume())
    playing = true
    stepOnce()

  def stopPlaying(): Unit =
    playing = false
    timerId.foreach(dom.window.clearTimeout(_))
    timerId = None
    rerender()

  def toggle(): Unit = if playing then stopPlaying() else startPlaying()

  def reset(): Unit =
    stopPlaying()
    history = Vector(seed)
    rerender()

  def setOrder(o: Int): Unit =
    order = o
    rerender()

  def setScale(s: Scale): Unit =
    scale = s
    rerender()

  def setTempo(bpm: Int): Unit =
    tempoBpm = bpm
    rerender()

  override def afterDetach(): Unit = stopPlaying()

  private def barChart(dist: Vector[(Note, Double)]): VHtmlContent =
    <.div(
      for (note, p) <- dist yield
        <.div(^.cls := "mz-bar-row",
          <.span(^.cls := "mz-bar-label", s"${noteName(note, scale)} (${note.duration.label})"),
          <.div(^.cls := "mz-bar-track", <.div(^.cls := "mz-bar-fill", ^.style := s"width: ${(p * 100).round}%;")),
          <.span(^.cls := "mz-bar-pct", s"${(p * 100).round}%")
        )
    )

  override protected def render =
    val ctx = context()
    val topNext = nextDistribution(ctx).sortBy(-_._2).take(6)
    val stateDescription = ctx.map(n => s"${noteName(n, scale)} (${n.duration.label})").mkString(" → ")

    <.div(^.cls := styling.className,
      <.p(^.cls := "br-label", "Generates a melody one note at a time from a Markov chain — order 1 or 2, major or minor, your choice."),

      <.div(^.cls := "mz-sequence",
        for (note, i) <- history.zipWithIndex yield
          <.div(^.cls := s"mz-note${if i >= history.size - order then " mz-current" else ""}",
            <.div(^.cls := "mz-note-char", noteName(note, scale)),
            <.div(^.cls := "mz-note-len", note.duration.label)
          )
      ),

      <.div(^.cls := "mz-state", s"Current state: $stateDescription"),

      <.div(^.cls := "mz-section-title", "Top predicted next notes"),
      barChart(topNext),

      <.div(^.cls := "mz-controls",
        <.button(^.cls := (if playing then "btn btn-warning btn-sm" else "btn btn-primary btn-sm"), ^.onClick --> toggle(), if playing then "Stop" else "▶ Play"),
        <.button(^.cls := "btn btn-outline-secondary btn-sm", ^.onClick --> reset(), "Reset"),

        <.span(^.cls := "mz-toggle-group",
          <.span(^.cls := "br-label", "Order: "),
          for o <- Vector(1, 2) yield
            <.span(^.cls := s"mz-toggle${if order == o then " mz-active" else ""}", ^.onClick --> setOrder(o), o.toString)
        ),

        <.span(^.cls := "mz-toggle-group",
          <.span(^.cls := "br-label", "Scale: "),
          for s <- Vector(Scale.Major, Scale.Minor) yield
            <.span(^.cls := s"mz-toggle${if scale == s then " mz-active" else ""}", ^.onClick --> setScale(s), s.toString)
        ),

        <.span(^.cls := "br-label", "Tempo"),
        <("input")(
          ^.attr("type") := "range", ^.attr("min") := "60", ^.attr("max") := "180", ^.attr("step") := "4",
          ^.attr("value") := tempoBpm.toString,
          ^.on("input") ==> { (e: dom.Event) =>
            setTempo(e.target.asInstanceOf[dom.html.Input].value.toInt)
          }
        ),
        <.span(^.cls := "br-label", s"$tempoBpm bpm")
      )
    )
}
