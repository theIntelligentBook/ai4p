package ai4p.uncertainty.widgets

import com.wbillingsley.veautiful.html.{<, SVG, Styling, DHtmlComponent, ^}
import org.scalajs.dom

import scala.util.Random

import ai4p.{*, given}
import Common.*

import site.given

object ParticleFilter1D {

  val w = 640
  val h = 200
  val pad = 30
  val trackY = 110

  def toSvgX(x: Double, worldLength: Double): Double =
    pad + x / worldLength * (w - 2 * pad)

  val styling = Styling(
    """|display: inline-block;
       |font-family: 'Lato', sans-serif;
       |""".stripMargin
  ).modifiedBy(
    " svg" -> "background: #f8f8f8; border: 1px solid #ddd; border-radius: 4px;",
    " .pf-controls" -> "margin-top: 8px; display: flex; gap: 8px; align-items: center; flex-wrap: wrap;",
    " .pf-info" -> "font-family: 'Playfair Design', serif; font-style: italic; font-size: 1rem; color: #5a074f; margin-top: 6px;",
    " .pf-badge" -> "font-size: 0.85rem; padding: 2px 8px; border-radius: 4px; font-weight: bold;",
    " .pf-badge.healthy" -> "background: #dcfce7; color: #166534;",
    " .pf-badge.low" -> "background: #fee2e2; color: #991b1b;",
    " .pf-label" -> "font-size: 0.85rem; color: #666;",
    " input[type=range]" -> "width: 120px;"
  ).register()
}

/**
 * A 1D "robot in a hallway" particle filter demo. The robot (and every
 * particle) shuffles back and forth along a line dotted with unevenly-spaced
 * doors. Sensing measures noisy distance to the *nearest* door, which is
 * often ambiguous between two mirror-image locations until the next motion
 * step breaks the symmetry (every particle is nudged by the same commanded
 * step, so only the hypothesis matching the true robot survives resampling).
 */
case class ParticleFilter1D(
  worldLength: Double = 22.0,
  landmarks: Vector[Double] = Vector(4.0, 10.0, 18.0),
  n: Int = 150,
  moveStep: Double = 1.6,
  motionNoiseStd: Double = 0.5
) extends DHtmlComponent {

  import ParticleFilter1D._

  def distToNearestLandmark(x: Double): Double = landmarks.map(l => math.abs(l - x)).min

  def clamp(x: Double): Double = math.max(0.0, math.min(worldLength, x))

  def freshParticles(): Array[Double] = Array.fill(n)(Random.nextDouble() * worldLength)

  var trueX: Double = Random.between(2.0, worldLength - 2.0)
  var direction: Int = if Random.nextBoolean() then 1 else -1
  var particles: Array[Double] = freshParticles()
  var sensorNoiseStd: Double = 0.6
  var lastReading: Option[Double] = None
  var ess: Double = n.toDouble
  var stepCount: Int = 0
  var autoRunning = false
  var autoPhase = 0
  var timerId: Option[Int] = None

  def gaussianLikelihood(diff: Double, std: Double): Double =
    math.exp(-0.5 * (diff * diff) / (std * std))

  def move(): Unit =
    val proposed = trueX + direction * moveStep
    if proposed < 0 || proposed > worldLength then direction = -direction
    trueX = clamp(trueX + direction * moveStep + Random.nextGaussian() * motionNoiseStd)
    particles = particles.map(p => clamp(p + direction * moveStep + Random.nextGaussian() * motionNoiseStd))
    stepCount += 1
    rerender()

  def senseAndResample(): Unit =
    val reading = distToNearestLandmark(trueX) + Random.nextGaussian() * sensorNoiseStd
    lastReading = Some(reading)
    val rawWeights = particles.map(p => gaussianLikelihood(reading - distToNearestLandmark(p), sensorNoiseStd) + 1e-9)
    val total = rawWeights.sum
    val weights = rawWeights.map(_ / total)
    ess = 1.0 / weights.map(wt => wt * wt).sum
    particles = systematicResample(particles, weights)
    stepCount += 1
    rerender()

  def systematicResample(ps: Array[Double], weights: Array[Double]): Array[Double] =
    val m = ps.length
    val cumulative = weights.scanLeft(0.0)(_ + _).tail
    val out = new Array[Double](m)
    var j = 0
    for i <- 0 until m do
      val target = (i + Random.nextDouble()) / m
      while j < m - 1 && cumulative(j) < target do j += 1
      out(i) = ps(j)
    out

  def reset(): Unit =
    stopAuto()
    trueX = Random.between(2.0, worldLength - 2.0)
    direction = if Random.nextBoolean() then 1 else -1
    particles = freshParticles()
    lastReading = None
    ess = n.toDouble
    stepCount = 0
    rerender()

  def stopAuto(): Unit =
    timerId.foreach(dom.window.clearInterval(_))
    timerId = None
    autoRunning = false

  def toggleAuto(): Unit =
    if autoRunning then
      stopAuto()
      rerender()
    else
      autoRunning = true
      timerId = Some(dom.window.setInterval(() => {
        if autoPhase == 0 then move() else senseAndResample()
        autoPhase = 1 - autoPhase
      }, 700))
      rerender()

  override def afterDetach(): Unit = stopAuto()

  override protected def render =
    val estimate = particles.sum / particles.length
    val essFraction = ess / n
    val essStatus = if essFraction < 0.5 then "low" else "healthy"
    val essLabel = if essFraction < 0.5 then "concentrating — needs resampling" else "healthy diversity"

    <.div(^.cls := styling.className,
      SVG.svg(^.attr("width") := w, ^.attr("height") := h,

        // Track
        SVG.line(^.attr("x1") := toSvgX(0, worldLength), ^.attr("y1") := trackY,
                 ^.attr("x2") := toSvgX(worldLength, worldLength), ^.attr("y2") := trackY,
                 ^.attr("stroke") := "#999", ^.attr("stroke-width") := "3"),

        // Landmarks (doors)
        for lm <- landmarks yield
          SVG.g(
            SVG.line(^.attr("x1") := toSvgX(lm, worldLength), ^.attr("y1") := trackY - 14,
                     ^.attr("x2") := toSvgX(lm, worldLength), ^.attr("y2") := trackY + 14,
                     ^.attr("stroke") := "#f59e0b", ^.attr("stroke-width") := "4"),
            SVG.text(^.attr("x") := toSvgX(lm, worldLength), ^.attr("y") := trackY + 30,
                     ^.attr("text-anchor") := "middle", ^.attr("font-size") := "16", "🚪")
          ),

        // Particle cloud — density along the track shows the belief
        for (p, i) <- particles.zipWithIndex yield
          val jitter = ((i % 9) - 4) * 2.2
          SVG.circle(
            ^.attr("cx") := toSvgX(p, worldLength), ^.attr("cy") := trackY - 26 + jitter,
            ^.attr("r") := "3", ^.attr("fill") := "#3b82f6", ^.attr("opacity") := "0.5"
          ),

        // Weighted estimate marker
        SVG.line(^.attr("x1") := toSvgX(estimate, worldLength), ^.attr("y1") := trackY - 52,
                 ^.attr("x2") := toSvgX(estimate, worldLength), ^.attr("y2") := trackY - 4,
                 ^.attr("stroke") := "#16a34a", ^.attr("stroke-width") := "2", ^.attr("stroke-dasharray") := "4 2"),
        SVG.text(^.attr("x") := toSvgX(estimate, worldLength), ^.attr("y") := trackY - 56,
                 ^.attr("text-anchor") := "middle", ^.attr("font-size") := "11", ^.attr("fill") := "#16a34a", "estimate"),

        // True robot
        SVG.text(^.attr("x") := toSvgX(trueX, worldLength), ^.attr("y") := trackY + 52,
                 ^.attr("text-anchor") := "middle", ^.attr("font-size") := "22", "🤖")
      ),

      <.div(^.cls := "pf-info",
        lastReading match
          case Some(r) => f"Last sensor reading: $r%.2f (true nearest-door distance: ${distToNearestLandmark(trueX)}%.2f) — step $stepCount"
          case None => s"Press Move, then Sense & Resample — step $stepCount"
      ),

      <.div(^.cls := "pf-controls",
        <.button(^.cls := "btn btn-outline-primary btn-sm", ^.onClick --> move(), "Move"),
        <.button(^.cls := "btn btn-outline-primary btn-sm", ^.onClick --> senseAndResample(), "Sense & Resample"),
        <.button(^.cls := (if autoRunning then "btn btn-warning btn-sm" else "btn btn-outline-success btn-sm"),
          ^.onClick --> toggleAuto(), if autoRunning then "Stop" else "Auto"),
        <.button(^.cls := "btn btn-outline-secondary btn-sm", ^.onClick --> reset(), "Reset"),

        <.span(^.cls := "pf-label", "sensor noise σ ="),
        <("input")(
          ^.attr("type") := "range", ^.attr("min") := "0.1", ^.attr("max") := "2.0", ^.attr("step") := "0.1",
          ^.attr("value") := sensorNoiseStd.toString,
          ^.on("input") ==> { (e: dom.Event) =>
            sensorNoiseStd = e.target.asInstanceOf[dom.html.Input].value.toDouble
            rerender()
          }
        ),
        <.span(^.cls := "pf-label", f"$sensorNoiseStd%.1f"),

        <.span(^.cls := s"pf-badge $essStatus", f"ESS ${ess}%.0f/$n — $essLabel")
      )
    )
}
