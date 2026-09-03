package ai4p.neuralnets.widgets

import com.wbillingsley.veautiful.html.{SVG, DSvgContent, ^}
import org.scalajs.dom

/** A small square grid of on/off pixels, rendered as SVG - shared by the digit-recognition widgets. */
object PixelGrid {

  /** @param editable when true, clicking a cell calls `onToggle` with its index. */
  def svg(pixels: Vector[Int], gridSize: Int, cellPx: Int = 16, gap: Int = 2,
          editable: Boolean = false, onToggle: Int => Unit = _ => ()): DSvgContent =
    val size = gridSize * (cellPx + gap) + gap
    SVG.svg(^.attr("width") := size, ^.attr("height") := size,
      SVG.g(
        for i <- pixels.indices yield
          val row = i / gridSize; val col = i % gridSize
          SVG.rect(
            ^.attr("x") := gap + col * (cellPx + gap), ^.attr("y") := gap + row * (cellPx + gap),
            ^.attr("width") := cellPx, ^.attr("height") := cellPx, ^.attr("rx") := 2,
            ^.attr("fill") := (if pixels(i) == 1 then "#1d4ed8" else "#f1f5f9"),
            ^.attr("stroke") := "#94a3b8",
            ^.attr("style") := (if editable then "cursor:pointer;" else ""),
            ^.on("click") ==> { (_: dom.Event) => if editable then onToggle(i) }
          )
      )
    )
}
