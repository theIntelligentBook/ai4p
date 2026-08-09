package ai4p.uncertainty.widgets

import com.wbillingsley.veautiful.html.{<, SVG, Styling, DHtmlComponent, ^}
import org.scalajs.dom

import scala.util.Random

import ai4p.{*, given}
import Common.*

import site.given

object ParticleFilter2D {

  case class Pose(x: Double, y: Double, theta: Double)

  val pxW = 660
  val pxH = 420
  val pad = 30
  val scale = 20.0 // pixels per world unit — chosen so both axes line up exactly

  def toSvgX(x: Double): Double = pad + x * scale
  def toSvgY(y: Double): Double = pad + y * scale

  val styling = Styling(
    """|display: inline-block;
       |font-family: 'Lato', sans-serif;
       |""".stripMargin
  ).modifiedBy(
    " svg" -> "background: #f8f8f8; border: 1px solid #ddd; border-radius: 4px;",
    " .pf2-controls" -> "margin-top: 8px; display: flex; gap: 8px; align-items: center; flex-wrap: wrap;",
    " .pf2-info" -> "font-family: 'Playfair Design', serif; font-style: italic; font-size: 1rem; color: #5a074f; margin-top: 6px;",
    " .pf2-badge" -> "font-size: 0.85rem; padding: 2px 8px; border-radius: 4px; font-weight: bold;",
    " .pf2-badge.healthy" -> "background: #dcfce7; color: #166534;",
    " .pf2-badge.low" -> "background: #fee2e2; color: #991b1b;",
    " .pf2-label" -> "font-size: 0.85rem; color: #666;",
    " input[type=range]" -> "width: 120px;"
  ).register()
}

/**
 * A 2D room-localization particle filter demo — think of a robot vacuum cleaner finding its way
 * around a room. The robot wanders around, bouncing off walls; sensing gives a noisy distance
 * reading to every fixed object it can see at once (like a piece of furniture, not a radio
 * transmitter), which is enough to pin down both position and, over a couple of steps, heading.
 */
case class ParticleFilter2D(
  worldW: Double = 30.0,
  worldH: Double = 18.0,
  obstacles: Vector[(Double, Double)] = Vector((4.0, 4.0), (26.0, 4.0), (4.0, 14.0), (26.0, 14.0)),
  n: Int = 200,
  moveStep: Double = 2.0
) extends DHtmlComponent {

  import ParticleFilter2D._

  def clampX(x: Double): Double = math.max(0.0, math.min(worldW, x))
  def clampY(y: Double): Double = math.max(0.0, math.min(worldH, y))
  def normalizeAngle(a: Double): Double = math.atan2(math.sin(a), math.cos(a))

  def freshPose(): Pose = Pose(Random.nextDouble() * worldW, Random.nextDouble() * worldH, Random.nextDouble() * 2 * math.Pi - math.Pi)
  def freshParticles(): Array[Pose] = Array.fill(n)(freshPose())

  var truePose: Pose = Pose(worldW / 2, worldH / 2, Random.nextDouble() * 2 * math.Pi - math.Pi)
  var particles: Array[Pose] = freshParticles()
  var sensorNoiseStd: Double = 0.7
  var lastReadings: Vector[Double] = Vector.empty
  var ess: Double = n.toDouble
  var stepCount: Int = 0
  var autoRunning = false
  var autoPhase = 0
  var timerId: Option[Int] = None

  /** Moves a pose forward along its heading, bouncing off walls, plus noise. */
  def stepPose(p: Pose, posNoiseStd: Double, headingNoiseStd: Double): Pose =
    var theta = p.theta
    var nx = p.x + math.cos(theta) * moveStep
    var ny = p.y + math.sin(theta) * moveStep
    if nx < 0 || nx > worldW then
      theta = normalizeAngle(math.Pi - theta)
      nx = p.x + math.cos(theta) * moveStep
    if ny < 0 || ny > worldH then
      theta = normalizeAngle(-theta)
      ny = p.y + math.sin(theta) * moveStep
    val jx = Random.nextGaussian() * posNoiseStd
    val jy = Random.nextGaussian() * posNoiseStd
    val jt = Random.nextGaussian() * headingNoiseStd
    Pose(clampX(nx + jx), clampY(ny + jy), normalizeAngle(theta + jt))

  def move(): Unit =
    truePose = stepPose(truePose, posNoiseStd = 0.05, headingNoiseStd = 0.03)
    particles = particles.map(p => stepPose(p, posNoiseStd = 0.4, headingNoiseStd = 0.25))
    stepCount += 1
    rerender()

  def senseAndResample(): Unit =
    val trueDists = obstacles.map((bx, by) => math.hypot(truePose.x - bx, truePose.y - by) + Random.nextGaussian() * sensorNoiseStd)
    lastReadings = trueDists
    val logWeights = particles.map { p =>
      obstacles.zip(trueDists).map { case ((bx, by), obs) =>
        val pred = math.hypot(p.x - bx, p.y - by)
        val diff = obs - pred
        -0.5 * (diff * diff) / (sensorNoiseStd * sensorNoiseStd)
      }.sum
    }
    val maxLog = logWeights.max
    val rawWeights = logWeights.map(lw => math.exp(lw - maxLog) + 1e-9)
    val total = rawWeights.sum
    val weights = rawWeights.map(_ / total)
    ess = 1.0 / weights.map(wt => wt * wt).sum
    particles = systematicResample(particles, weights)
    stepCount += 1
    rerender()

  def systematicResample(ps: Array[Pose], weights: Array[Double]): Array[Pose] =
    val m = ps.length
    val cumulative = weights.scanLeft(0.0)(_ + _).tail
    val out = new Array[Pose](m)
    var j = 0
    for i <- 0 until m do
      val target = (i + Random.nextDouble()) / m
      while j < m - 1 && cumulative(j) < target do j += 1
      out(i) = ps(j)
    out

  def reset(): Unit =
    stopAuto()
    truePose = Pose(worldW / 2, worldH / 2, Random.nextDouble() * 2 * math.Pi - math.Pi)
    particles = freshParticles()
    lastReadings = Vector.empty
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
    val estX = particles.map(_.x).sum / particles.length
    val estY = particles.map(_.y).sum / particles.length
    val essFraction = ess / n
    val essStatus = if essFraction < 0.5 then "low" else "healthy"
    val essLabel = if essFraction < 0.5 then "concentrating — needs resampling" else "healthy diversity"

    <.div(^.cls := styling.className,
      SVG.svg(^.attr("width") := pxW, ^.attr("height") := pxH,

        // Room walls
        SVG.rect(^.attr("x") := pad, ^.attr("y") := pad,
          ^.attr("width") := worldW * scale, ^.attr("height") := worldH * scale,
          ^.attr("fill") := "none", ^.attr("stroke") := "#999", ^.attr("stroke-width") := "3"),

        // Obstacles — fixed objects in the room the robot can see and range to
        for (bx, by) <- obstacles yield
          SVG.rect(
            ^.attr("x") := toSvgX(bx) - 7, ^.attr("y") := toSvgY(by) - 7,
            ^.attr("width") := 14, ^.attr("height") := 14, ^.attr("rx") := 2,
            ^.attr("fill") := "#78716c", ^.attr("stroke") := "#44403c", ^.attr("stroke-width") := "1.5"
          ),

        // Particle cloud
        for p <- particles yield
          SVG.circle(
            ^.attr("cx") := toSvgX(p.x), ^.attr("cy") := toSvgY(p.y),
            ^.attr("r") := "2.5", ^.attr("fill") := "#3b82f6", ^.attr("opacity") := "0.45"
          ),

        // Weighted estimate marker
        SVG.g(
          SVG.line(^.attr("x1") := toSvgX(estX) - 7, ^.attr("y1") := toSvgY(estY) - 7,
                   ^.attr("x2") := toSvgX(estX) + 7, ^.attr("y2") := toSvgY(estY) + 7,
                   ^.attr("stroke") := "#16a34a", ^.attr("stroke-width") := "2"),
          SVG.line(^.attr("x1") := toSvgX(estX) - 7, ^.attr("y1") := toSvgY(estY) + 7,
                   ^.attr("x2") := toSvgX(estX) + 7, ^.attr("y2") := toSvgY(estY) - 7,
                   ^.attr("stroke") := "#16a34a", ^.attr("stroke-width") := "2")
        ),

        // True robot (triangle pointing along heading)
        SVG.g(^.attr("transform") := s"translate(${toSvgX(truePose.x)},${toSvgY(truePose.y)}) rotate(${truePose.theta * 180 / math.Pi})",
          SVG("polygon")(^.attr("points") := "10,0 -7,6 -7,-6", ^.attr("fill") := "#ef4444", ^.attr("stroke") := "white", ^.attr("stroke-width") := "1.5")
        )
      ),

      <.div(^.cls := "pf2-info",
        if lastReadings.isEmpty then s"Press Move, then Sense & Resample — step $stepCount"
        else f"Last ranges to objects: ${lastReadings.map(r => f"$r%.1f").mkString(", ")} — step $stepCount"
      ),

      <.div(^.cls := "pf2-controls",
        <.button(^.cls := "btn btn-outline-primary btn-sm", ^.onClick --> move(), "Move"),
        <.button(^.cls := "btn btn-outline-primary btn-sm", ^.onClick --> senseAndResample(), "Sense & Resample"),
        <.button(^.cls := (if autoRunning then "btn btn-warning btn-sm" else "btn btn-outline-success btn-sm"),
          ^.onClick --> toggleAuto(), if autoRunning then "Stop" else "Auto"),
        <.button(^.cls := "btn btn-outline-secondary btn-sm", ^.onClick --> reset(), "Reset"),

        <.span(^.cls := "pf2-label", "sensor noise σ ="),
        <("input")(
          ^.attr("type") := "range", ^.attr("min") := "0.1", ^.attr("max") := "3.0", ^.attr("step") := "0.1",
          ^.attr("value") := sensorNoiseStd.toString,
          ^.on("input") ==> { (e: dom.Event) =>
            sensorNoiseStd = e.target.asInstanceOf[dom.html.Input].value.toDouble
            rerender()
          }
        ),
        <.span(^.cls := "pf2-label", f"$sensorNoiseStd%.1f"),

        <.span(^.cls := s"pf2-badge $essStatus", f"ESS ${ess}%.0f/$n — $essLabel")
      )
    )
}
