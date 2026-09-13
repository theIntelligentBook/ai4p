package ai4p.embeddings.widgets

import com.wbillingsley.veautiful.html.{<, SVG, Styling, DHtmlComponent, DSvgContent, ^}
import org.scalajs.dom
import scala.concurrent.ExecutionContext.Implicits.global
import ai4p.{*, given}

/**
 * An *illustration* of what attention weights look like, built by re-using the same GloVe vectors
 * from [[EmbeddingModel]]: click a token to make it the "query", and every other token gets an
 * arc whose thickness/opacity is its softmax-weighted cosine similarity to that query.
 *
 * This is explicitly a stand-in, not real attention: real self-attention learns separate Query,
 * Key and Value projections of each token (so a token's role as a query can differ from its role
 * as a key), rather than just comparing raw embeddings to each other - and a real model's weights
 * come from training, not off-the-shelf similarity. But the *shape* of the result - a probability
 * distribution over "how much should I look at each other token" - is exactly the idea.
 */
object AttentionWidget {

  val presets: Vector[String] = Vector(
    "the bank raised interest rates again",
    "the river bank was flooded",
    "the tired dog chased the small cat",
    "the queen wore the golden crown"
  )

  val styling = Styling(
    """|display: inline-block;
       |font-family: 'Lato', sans-serif;
       |""".stripMargin
  ).modifiedBy(
    " .aw-controls" -> "margin-bottom: 10px; display: flex; align-items: center; gap: 8px; flex-wrap: wrap;",
    " select" -> "font-size: 0.85rem; padding: 2px 4px;",
    " .aw-token" -> "cursor: pointer; font-family: monospace; font-size: 14px; fill: #333;",
    " .aw-token.aw-query" -> "font-weight: bold; fill: #1d4ed8;",
    " .aw-token-right" -> "font-family: monospace; font-size: 14px; fill: #333;",
    " .aw-weight" -> "font-family: monospace; font-size: 10px; fill: #888;",
    " .aw-hint" -> "color: #666; font-size: 0.8rem; margin-top: 4px; max-width: 480px;",
    " .aw-warn" -> "color: #991b1b; font-size: 0.85rem;"
  ).register()

  private def softmax(xs: Vector[Double]): Vector[Double] =
    val m = xs.max
    val exps = xs.map(x => math.exp(x - m))
    val total = exps.sum
    exps.map(_ / total)
}

case class AttentionWidget() extends DHtmlComponent {
  import AttentionWidget._

  private var model: Option[EmbeddingModel] = None
  private var sentence: String = presets.head
  private var queryIndex: Int = 1

  EmbeddingModel.load().foreach { m => model = Some(m); rerender() }

  private def setSentence(s: String): Unit = { sentence = s; queryIndex = math.min(queryIndex, s.split(" ").length - 1); rerender() }
  private def setQuery(i: Int): Unit = { queryIndex = i; rerender() }

  // A fixed sharpness for the softmax - just enough to spread weight visibly across a handful of
  // words rather than winner-take-all or perfectly flat.
  private val temperature = 0.35

  private def weights(m: EmbeddingModel, tokens: Vector[String]): Vector[Double] =
    val vecs = tokens.map(t => m.vector(t).getOrElse(Array.fill(50)(0.0)))
    val qv = vecs(queryIndex)
    val sims = vecs.map(v => m.cosine(qv, v) / temperature)
    softmax(sims)

  // Laid out BertViz-style: the same tokens listed twice, in two columns (the query word on the
  // left, everything it can attend to on the right), joined by curves whose thickness/opacity is
  // the attention weight - rather than a single row with a fan of lines above it.
  private def diagram(m: EmbeddingModel) =
    val tokens = sentence.split(" ").toVector
    val n = tokens.size
    val rowHeight = 28
    val topPad = 34
    val w = 560; val h = topPad + (n - 1) * rowHeight + 20
    val leftTextX = 90; val curveLeftX = 100
    val curveRightX = 430; val rightTextX = 440; val weightX = 520
    val ys = Vector.tabulate(n)(i => topPad + i * rowHeight)
    val ws = weights(m, tokens)

    val curves: Vector[DSvgContent] = tokens.indices.toVector.map { i =>
      val weight = ws(i)
      val opacity = math.max(0.04, weight)
      val strokeWidth = 0.5 + weight * 7
      val y1 = ys(queryIndex); val y2 = ys(i)
      val midX = (curveLeftX + curveRightX) / 2.0
      SVG.path(
        ^.attr("d") := s"M $curveLeftX,$y1 C $midX,$y1 $midX,$y2 $curveRightX,$y2",
        ^.attr("fill") := "none", ^.attr("stroke") := "#1d4ed8",
        ^.attr("stroke-opacity") := opacity, ^.attr("stroke-width") := strokeWidth
      )
    }

    val leftLabels: Vector[DSvgContent] = tokens.indices.toVector.map { i =>
      SVG.text(^.cls := s"aw-token${if i == queryIndex then " aw-query" else ""}",
        ^.attr("x") := leftTextX, ^.attr("y") := ys(i) + 4, ^.attr("text-anchor") := "end",
        ^.on("click") ==> { (_: dom.Event) => setQuery(i) },
        tokens(i))
    }

    val rightLabels: Vector[DSvgContent] = tokens.indices.toVector.map { i =>
      SVG.text(^.cls := "aw-token-right",
        ^.attr("x") := rightTextX, ^.attr("y") := ys(i) + 4, ^.attr("text-anchor") := "start",
        tokens(i))
    }

    val weightLabels: Vector[DSvgContent] = tokens.indices.toVector.map { i =>
      SVG.text(^.cls := "aw-weight",
        ^.attr("x") := weightX, ^.attr("y") := ys(i) + 4, ^.attr("text-anchor") := "start",
        f"${ws(i)}%.2f")
    }

    val headers = SVG.g(
      SVG.text(^.attr("x") := leftTextX, ^.attr("y") := 16, ^.attr("text-anchor") := "end",
        ^.attr("font-size") := "11", ^.attr("fill") := "#888", ^.attr("font-family") := "'Lato', sans-serif", "query"),
      SVG.text(^.attr("x") := rightTextX, ^.attr("y") := 16, ^.attr("text-anchor") := "start",
        ^.attr("font-size") := "11", ^.attr("fill") := "#888", ^.attr("font-family") := "'Lato', sans-serif", "attends to")
    )

    SVG.svg(^.attr("width") := w, ^.attr("height") := h,
      headers,
      SVG.g(curves*),
      SVG.g(leftLabels*),
      SVG.g(rightLabels*),
      SVG.g(weightLabels*)
    )

  override protected def render =
    val body = model match
      case None => <.div(^.cls := "aw-warn", "Loading word vectors...")
      case Some(m) =>
        <.div(
          <.div(^.cls := "aw-controls",
            "sentence:",
            <.select(
              ^.on("change") ==> { (e: dom.Event) => setSentence(e.target.asInstanceOf[dom.html.Select].value) },
              for p <- presets yield
                <.option(^.attr("value") := p, ^.attr("selected") ?= (if p == sentence then Some("selected") else None), p)
            )
          ),
          diagram(m),
          <.div(^.cls := "aw-hint",
            "Click a word on the left to make it the query - the curves to the right show how much weight it puts on each other word. (This uses plain cosine similarity of static vectors as a stand-in for learned attention - see the next slide for how real attention differs.)")
        )
    <.div(^.cls := styling.className, body)
}
