package ai4p.reasoning
import com.wbillingsley.veautiful.html.*
import com.wbillingsley.veautiful.doctacular.DeckBuilder

import <._
import ^._

import ai4p.{*, given}
import Common.*

import site.given
import scala.util.Random

import site.given
import scala.util.Random

import org.scalajs.dom
import ai4p.reasoning.queens.QueensSelector
import circuitsup.mosfets.PMos.*
import com.wbillingsley.wren.*
import circuitsup.analog.Ohms.*
import circuitsup.templates.ScatterPlot



// Ported from https://github.com/theIntelligentBook/circuitsup/blob/master/wren.css
// Comma-separated selectors in the original stylesheet are split into separate
// modifiedBy entries so that every selector stays scoped under wrenStyle.className
// (a bare comma-joined selector would otherwise leak the second half unscoped).
val wrenStyle = Styling("font-size: 20px; stroke: black;").modifiedBy(
  " .wren-component" -> "fill: none; stroke: black; stroke-width: 1.7; ",
  " .wren-component text" -> "fill: black; stroke: none;",
  " .wren-component text.centre.middle" -> "dominant-baseline: middle; text-anchor: middle;",
  " .wren-component text.centre" -> "text-anchor: middle;",
  " .wren-component text.right" -> "text-anchor: end;",
  " .wren-component .current-source-arrow" -> "font-size: 36px; font-weight: bolder;",
  " .wren-canvas circle.terminal" -> "fill: black; r: 2;",
  " .current-arrow" -> "stroke: gray(0.8); fill: none; stroke-width: 1;",
  " .voltage-markers" -> "text-anchor: middle; dominant-baseline: middle;",
  " .value-slider" -> "width: 100px;",
  " .value-label.green text" -> "stroke: green;",
  " .logic-probe text.display" -> "text-anchor: middle; dominant-baseline: middle; font-size: 24px; font-family: monospace;",
  " .logic-probe text.output-name" -> "dominant-baseline: middle;",
  " .logic-probe path.red" -> "stroke: red;",
  " .logic-probe path.blue" -> "stroke: blue;",
  " .semiconductor" -> "fill: lightgray;",
  " .semiconductor.ndoped" -> "fill: pink;",
  " .semiconductor.pdoped" -> "fill: yellowgreen;",
  " .semiconductor.glass" -> "fill: cadetblue;",
  " .semiconductor.channel" -> "stroke: none;",
  " .semiconductor-connector" -> "stroke-width: 5px;",
  " .nmos-switch path.red" -> "stroke: red;",
  " .nmos-switch circle.red" -> "stroke: red;",
  " .nmos-switch path.blue" -> "stroke: blue;",
  " .nmos-switch circle.blue" -> "stroke: blue;",
  " .pmos-switch path.red" -> "stroke: red;",
  " .pmos-switch circle.red" -> "stroke: red;",
  " .pmos-switch path.blue" -> "stroke: blue;",
  " .pmos-switch circle.blue" -> "stroke: blue;",
  " .wire.red" -> "stroke: red;",
  " .wire.blue" -> "stroke: blue;",
  " .logic-input button" -> "box-sizing: border-box; width: 30px; height: 40px; border: 4px double black; border-radius: 8px;",
  " .logic-input button:hover" -> "background: powderblue;",
  " .logic-input path.red" -> "stroke: red;",
  " .logic-input path.blue" -> "stroke: blue;",
  " .logic-input text.input-name" -> "text-anchor: end; dominant-baseline: middle;",
  " .half-adder text.input" -> "dominant-baseline: middle;",
  " .half-adder text.output" -> "text-anchor: end; dominant-baseline: middle;",
  " .half-adder text.name" -> "text-anchor: middle; dominant-baseline: middle; font-size: 1.2rem;",
  " .flip-flop text.input" -> "dominant-baseline: middle;",
  " .flip-flop text.output" -> "text-anchor: end; dominant-baseline: middle;",
  " table.truth-table" -> "border: 1px solid black; margin-bottom: 1rem;",
  " table.truth-table th" -> "text-align: center; line-height: 1.5rem; padding: 5px;",
  " table.truth-table td" -> "text-align: center; line-height: 1.5rem; padding: 5px;",
  " table.truth-table tbody tr" -> "border-top: 1px solid gray;",
  " table.truth-table tbody tr:nth-child(2n)" -> "background: gainsboro;",
  " table.truth-table tbody tr.active" -> "background: moccasin;",
  " .binary-table" -> "border: 1px solid black; padding: 5px; border-radius: 5px; display: inline-table;",
  " .binary-table th" -> "background: lightgray; padding: 10px; text-align: center; min-width: 40px;",
  " .binary-table td" -> "font-family: monospace; padding: 5px; font-size: 20px;",
  " .binary-table th.hex-string" -> "background: bisque; border-left: 1px solid black; padding-left: 10px; padding-right: 10px;",
  " .binary-table td.hex-string" -> "background: bisque; border-left: 1px solid black; padding-left: 10px; padding-right: 10px; font-style: italic;",
  " .binary-table th.decimal-string" -> "background: lightblue; border-left: 1px solid black; padding-left: 10px; padding-right: 10px;",
  " .binary-table td.decimal-string" -> "background: lightblue; border-left: 1px solid black; padding-left: 10px; padding-right: 10px; font-style: italic; color: gray;",
  " .binary-table th.target-string" -> "background: orange; border-left: 1px solid black; padding-left: 10px; padding-right: 10px;",
  " .binary-table td.target-string" -> "background: orange; border-left: 1px solid black; padding-left: 10px; padding-right: 10px;",
  " .binary-table th.target-string.complete" -> "background: lightgreen;",
  " .binary-table td.target-string.complete" -> "background: lightgreen; font-style: italic; color: gray;",
  " .binary-table td.nibble-end:not(:first-child)" -> "border-right: 1px solid black;",
  " .binary-table th.nibble-end:not(:first-child)" -> "border-right: 1px solid black;",
  " .logic-plot .logic-line" -> "margin-bottom: 1rem;",
  " .logic-plot svg" -> "margin: 1rem; background: lightgray;",
  " .logic-plot .logic-line path" -> "stroke: blue; fill: none;",
  " .scatterplot .tick-label-y" -> "dominant-baseline: end; text-anchor: end; font-size: 0.9em;",
  " .scatterplot .tick-label-x" -> "text-anchor: middle; font-size: 0.9em;",
).register()

case class Circt() extends DHtmlComponent {
    val nmc = new PMOSFETCircuit()
    nmc.vdd.voltage.content = Some(3d -> UserSet)
    nmc.vgb.voltage.content = Some(3d -> UserSet)

    val vg = new ValueLabel("V" -> "g", nmc.pMosfet.gate.potential, 200 -> 275, "centre", symbol=Seq(ValueLabel.voltageMarkers(200 -> 220, 200 -> 330)))


    val circuit = new Circuit(nmc.components :+
      new ValueSlider(nmc.vdd.voltage, nmc.vdd.x + 30 -> (nmc.vdd.y + 10), max="5", min="0", step="0.1")(() => onUpdate()) :+
      new ValueSlider(nmc.vgb.voltage, nmc.vgb.x - 130 -> (nmc.vgb.y + 10), max="5", min="0", step="0.1")(() => {
        plotData.data.clear()
        plotData.update()
        onUpdate()
      }) :+ vg, 600, 400)
    val propagator = new ConstraintPropagator(circuit.components.flatMap(_.constraints))
    propagator.resolve()

    val plotData = new ScatterPlotData(nmc.vdd.voltage, nmc.pMosfet.drain.current)

    def checkCompletion:Boolean = plotData.dataValues().length > 25

    def onUpdate():Unit = {
      propagator.clearCalculations()
      propagator.resolve()
      plotData.update()
      rerender()
    }

    def render = <.div(^.cls := wrenStyle.className,
      circuit,
      ScatterPlot(600, 300, "Vdd", "Current", (d) => nmc.vdd.voltage.stringify(d), (d) => nmc.pMosfet.drain.current.stringify(d), 5,0.005).plot(plotData.dataValues())
    )
    
}

val reasoningDeck = DeckBuilder(1920, 1080)
  .markdownSlide(
    """
      |# Reasoning and Logic
      |
      |""".stripMargin
  ).withClass("center middle")
  .markdownSlides(
    """## A logic puzzle
      |
      |There is a riddle, that's variously been attributed to Albert Einstein, Lewis Carroll, and others. There are 
      |many variations, but one of them goes:
      |
      |> In my street, there are five houses.
      |The Englishman lives in the red house.
      |> The Swede keeps dogs.
      |> The Dane drinks tea.
      |> The green house is to the left of the white house.
      |> The owner of the green house drinks coffee.
      |> The owner of the yellow house smokes Dunhills.
      |> The man in the middle house drinks milk.
      |> The Norwegian lives in the first house.
      |> The Blends smoker has a neighbour who has a cat.
      |> The man who smokes BlueMasters drinks beer.
      |> The man who keeps horses lives next to the man who smokes Dunhills.
      |> The German smokes Princes.
      |> The Norwegian lives next to the blue house.
      |> The Blends smoker has a neighbour who drinks water.
      |>
      |> So, who owns the fish?
      |
      |This isn't going to be solved by machine learning, because there's nothing really to train on.
      |
      |I suppose we *could* solve this by searching every single possibility, but perhaps we should solve it as intended: using logical reasoning.
      |
      |---
      |
      |## Making this one easy...
      |
      |I set this one as an assignment for functional programmers, and the way I recommended doing it is to lay out all the possibilities
      |for each house.
      |
      |Then, we use logic rules to progressively eliminate possibilities by what's *not* possible.
      |
      |
      |""".stripMargin
  )
  .veautifulSlide(<.div(
    <.h2("Einstein's Problem"),
    <.p(renderStreet(Street()))
  ))
  .veautifulSlide(<.div(
    <.h2("Einstein's Problem"),
    <.p(StreetWidget(Street()))
  ))
  .markdownSlides(
    """
    |## LinkedIn Queens
    |
    |Games like Sudoku are similar, in that they are about what's possible and logical rules for what isn't.
    |
    |LinkedIn have a puzzle game called "Queens" that's loosely based on the "eight queens problem" 
    |
    |* You're going to be shown a grid with some coloured regions. 
    |* There can only be 1 queen in each:
    |  - column
    |  - row
    |  - coloured region 
    |
    |""".stripMargin)
  .veautifulSlide(<.div(
    <.h2("LinkedIn Queens"),
    <.p(QueensSelector())
  ))
  .markdownSlides(
    """
    |## The story so far
    |
    |There's a class of problem where we want to move from a state to another state that's closer to the goal, based on logical steps
    |
    |Defining what the steps are is often part of the problem. 
    |
    |Here, we've enumerated possibilities in each cell and are eliminating them, but sometimes it's about filling in other unknowns and chaining forward
    |
    |Let's go to a couple of examples from way back in my PhD...
    |
    |""".stripMargin)
  .imageSlide("MIT 6.001x circuits exercise, 2003", "images/mitcircuitsex.png")
  .imageSlide("Generated explanation from a constraint propagator, 2003", "images/constraintpropagator.jpg")
  .imageSlide("Intelligent Book explaining a constraing propagator step, 2004", "images/tibcircuits.jpg")
  .imageSlide("Circuits Up!, 2020", "images/circuitsup.png")
  .veautifulSlide(<.div(
    <.h2("P-Channel MOSFET"),
    <.div(
        Circt()
    )
  ))
  .markdownSlides(
    """
    |## Chains of reasoning...
    |
    |At each step, we have a state representing what we know about the problem.
    |
    |We're looking for a step that will take us towards some goal.
    |
    |Each step of the reasoning has a very clear element of *why* it is true. It is a deduction, not just an arbitrary move.
    |
    |But, those deductions can still be long...
    |
    |---
    |
    |## Proofs and Programs
    |
    |The *Curry-Howard Correspondence* is that mathematical proofs can be written as computer programs.
    |
    |Some of my PhD work was on helping students with machine proofs in their program form...
    |
    |""".stripMargin)
  .imageSlide("A theorem proof in Isabelle/HOL", "images/hol.png")
  .veautifulSlide(<.div(
    <.h2("MathsTiles - a scatterable interface for mathematical proofs"),
    <.p(<.img(^.src := "images/mathstiles.png", ^.attr.width := "480"), <.br(), <.img(^.src := "images/prooftile.png", ^.attr.width := "600"))
  ))
  .imageSlide("A theorem proof using MathsTiles and Isabelle/HOL", "images/ibproof.png")
  .markdownSlides(
    """
    |## It works, but...
    |
    |* Theorem prover "tactics" are searches for rules that will take you towards the goal.
    |* It's not always clear what "towards the goal" means
    |* Sometimes the reasoner gets stuck. It can't prove the next line is true, but it can't prove it false either. Now what?
    |* The size of the search space is very big. Isabelle's simplifier had more than 1,500 rules in it.
    |
    |Educationally, AI can be a usability problem: 
    |
    |* When we interact with systems, we try to build a mental model of how they work
    |* An AI or machine reasoner can be a fantastically big and complicated system
    |* So now your brain is trying to figure out a mental model of how a fantastically complicated system works...
    |
    |---
    |
    |## Artificial stupidity
    |
    |In AI in Education, one of the answers, I found, was to make the AI deliberately *stupid*. 
    |
    |"You can't help but know how this works" level of simplicity.
    |
    |That's because in education, we're not solving *abritrary* problems, we already have a way of thinking we want you to come to.
    |
    |""".stripMargin)
  .imageSlide("An informally modelled proof, 2007", "images/informalreasoning.png")
  .imageSlide("A theorem proof in Lean", "images/leanfibpositive.png")
  .imageSlide("Scatter - successor to MathsTiles, 2020", "images/scatter.png")
  .markdownSlides(
    """
    |## Strategy vs Tactics
    |
    |Skmething we found with the circuits example is there's a difference between strategy and tactics
    |
    |* The reasoning system works at a tactical level, figuring out every consequence logically
    |* Solving the problem though, was much easier if you started in one place (to the right of the transistor) than another (to the left of the transistor)
    |
    |That "It's much easier if..." is a strategic decision that doesn't have a lot of logic to it except knowing it works.
    |
    |It's also the kind of advice that a Large Language Model (LLM) could pick up very easily.
    |
    |... and an **enormous** amount of success that AI has had recently has been where an LLM is paired with some kind of deduction or verification model.
    |
    |---
    |
    |## Why an LLM + verification...
    |
    |LLMs express strategy pragmatically, based on what works and based on learning from human output
    |
    |We have *no idea* how much of the rules of the domain they've really internalised. Hence why we keep catching them out with trick questions.
    |
    |The verification model (compiler, or theorem prover) has been explicitly coded with the rules of the domain and produces good error messages when something goes wrong.
    |
    |That gives a neat feedback loop:
    |
    |* A "loose" but capable LLM producing its best guess
    |* A verifier finding out precisely what's wrong with it
    |* The AI being able to take the correction message and try again *quickly*.
    | 
    |""".stripMargin)
  .imageSlide("An LLM and a verifier working in a loop - Claude Code", "images/claudecoderecompile.png")
  .imageSlide("OpenAI solves Unit Distance Problem", "images/unitdistanceproblem.jpg")
  .markdownSlide(willCcBy)
  .renderSlides
