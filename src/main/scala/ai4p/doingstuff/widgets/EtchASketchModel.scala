package ai4p.doingstuff.widgets

import scala.scalajs.js
import scala.util.Try

/**
 * The "drawing API" itself - immutable state for a pen-plotter, styled after a real Etch-A-Sketch:
 * two knobs (left/right, up/down), and no way to draw except by moving the pen while it's down.
 * There's deliberately no "turn" or "move to" command - just the same four directions an Etch-A-Sketch
 * gives you - so the whole API is five tiny functions, small enough to hand an LLM as a tool list.
 */
case class EtchASketchModel(
  x: Double = 0,
  y: Double = 0,
  isPenDown: Boolean = false,
  strokes: Vector[Vector[(Double, Double)]] = Vector.empty
) {

  def penUp(): EtchASketchModel = copy(isPenDown = false)

  def penDown(): EtchASketchModel =
    if isPenDown then this
    else copy(isPenDown = true, strokes = strokes :+ Vector((x, y)))

  private def moveTo(nx: Double, ny: Double): EtchASketchModel =
    if isPenDown then copy(x = nx, y = ny, strokes = strokes.init :+ (strokes.last :+ (nx, ny)))
    else copy(x = nx, y = ny)

  def moveLeft(steps: Double): EtchASketchModel = moveTo(x - steps, y)
  def moveRight(steps: Double): EtchASketchModel = moveTo(x + steps, y)
  def moveUp(steps: Double): EtchASketchModel = moveTo(x, y - steps)
  def moveDown(steps: Double): EtchASketchModel = moveTo(x, y + steps)
}

object EtchASketchModel {

  val empty: EtchASketchModel = EtchASketchModel()

  /** One call against the drawing API - either issued by clicking a button, or parsed out of an LLM's reply. */
  enum Command(val toolName: String):
    case PenUp extends Command("pen_up")
    case PenDown extends Command("pen_down")
    case MoveLeft(steps: Double) extends Command("move_left")
    case MoveRight(steps: Double) extends Command("move_right")
    case MoveUp(steps: Double) extends Command("move_up")
    case MoveDown(steps: Double) extends Command("move_down")

  def apply(s: EtchASketchModel, cmd: Command): EtchASketchModel = cmd match
    case Command.PenUp => s.penUp()
    case Command.PenDown => s.penDown()
    case Command.MoveLeft(n) => s.moveLeft(n)
    case Command.MoveRight(n) => s.moveRight(n)
    case Command.MoveUp(n) => s.moveUp(n)
    case Command.MoveDown(n) => s.moveDown(n)

  def runAll(s: EtchASketchModel, cmds: Seq[Command]): EtchASketchModel = cmds.foldLeft(s)(apply)

  /**
   * The tool list, in the same shape (name / description / JSON-schema arguments) that a real MCP
   * server's `tools/list` response would use. There's no real server behind this demo - the "server"
   * is just this Scala file - but this is exactly the information one would advertise to a client.
   */
  val toolListJson: String =
    """[
      |  { "name": "pen_up", "description": "Lift the pen. Moves after this will NOT draw.", "inputSchema": { "type": "object", "properties": {} } },
      |  { "name": "pen_down", "description": "Lower the pen. Moves after this WILL draw a straight line.", "inputSchema": { "type": "object", "properties": {} } },
      |  { "name": "move_left", "description": "Slide the pen left by `steps` units.", "inputSchema": { "type": "object", "properties": { "steps": { "type": "number" } }, "required": ["steps"] } },
      |  { "name": "move_right", "description": "Slide the pen right by `steps` units.", "inputSchema": { "type": "object", "properties": { "steps": { "type": "number" } }, "required": ["steps"] } },
      |  { "name": "move_up", "description": "Slide the pen up by `steps` units.", "inputSchema": { "type": "object", "properties": { "steps": { "type": "number" } }, "required": ["steps"] } },
      |  { "name": "move_down", "description": "Slide the pen down by `steps` units.", "inputSchema": { "type": "object", "properties": { "steps": { "type": "number" } }, "required": ["steps"] } }
      |]""".stripMargin

  /** Pulls the first `[ ... ]` out of some text - LLMs love to wrap JSON in ```json fences or a sentence of preamble. */
  private def extractJsonArray(text: String): String =
    val start = text.indexOf('[')
    val end = text.lastIndexOf(']')
    if start >= 0 && end > start then text.substring(start, end + 1) else text

  /** Parses `[{"tool": "move_right", "args": {"steps": 40}}, ...]` back into [[Command]]s. */
  def parseCommands(text: String): Either[String, Vector[Command]] =
    Try {
      val arr = js.JSON.parse(extractJsonArray(text)).asInstanceOf[js.Array[js.Dynamic]]
      val builder = Vector.newBuilder[Command]
      var i = 0
      while i < arr.length do
        val call = arr(i)
        val name = call.selectDynamic("tool").asInstanceOf[String]
        val args = call.selectDynamic("args")
        def steps: Double =
          val v = args.selectDynamic("steps")
          if js.isUndefined(v) then throw new Exception(s"$name needs a \"steps\" argument")
          v.asInstanceOf[Double]
        builder += (name match
          case "pen_up" => Command.PenUp
          case "pen_down" => Command.PenDown
          case "move_left" => Command.MoveLeft(steps)
          case "move_right" => Command.MoveRight(steps)
          case "move_up" => Command.MoveUp(steps)
          case "move_down" => Command.MoveDown(steps)
          case other => throw new Exception(s"Unknown tool \"$other\" - not in the tool list I sent it")
        )
        i += 1
      builder.result()
    }.toEither.left.map(e => Option(e.getMessage).getOrElse(e.toString))
}
