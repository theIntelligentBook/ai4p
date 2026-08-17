package ai4p.uncertainty
import com.wbillingsley.veautiful.html.*
import com.wbillingsley.veautiful.doctacular.DeckBuilder

import <._
import ^._

import ai4p.{*, given}
import Common.*
import ai4p.uncertainty.widgets.{SpoofWidget, MorseMarkovWidget, MorseHmmWidget, MusicMarkovWidget}

import site.given


val morseTrie = Styling(
  """|font-family: monospace;
    |font-size: 15px;
    |padding: 2em;
    |""".stripMargin
  ).modifiedBy(
      " .tree" -> "color: #888; white-space: pre; line-height: 1.6;",
      " .morse" -> "font-weight: bold; color: black; letter-spacing: 0.15em;",
  ).register()

val markovModels = DeckBuilder(1920, 1080)
  .markdownSlide(
    """
      |# Markov Models and Hidden Markov Models
      |
      |""".stripMargin
  ).withClass("center middle")
  .veautifulSlide(<.div(
    <.h2("Sounds familiar?"),
    markdown.div(
      """|A taste of where this topic is headed, before we slow down and build it up from scratch.
         |This widget generates a melody one note at a time, always sampling the next note from a
         |probability distribution that depends only on the *current* state — the last one or two
         |notes played.
         |
         |That's the same basic move — predict the next thing from just enough recent context —
         |that sits underneath modern generative AI. A large language model predicting the next word
         |of a sentence is doing a vastly more sophisticated version of exactly the trick you're
         |about to watch happen with eight notes and a handful of hand-written rules.
         |
         |Try switching the **order** (does it remember just the last note, or the last two?) and the
         |**scale**, and watch the "top predicted next notes" panel update live as it plays.
         |""".stripMargin
    ),
    <.p(MusicMarkovWidget())
  ))
  .markdownSlide(
    """
      |
      |<div class="tree"><span class="morse">START</span>
      |├── <span class="morse">·</span> <span class="letter">(E)</span>
      |│   ├── <span class="morse">··</span> <span class="letter">(I)</span>
      |│   │   ├── <span class="morse">···</span> <span class="letter">(S)</span>
      |│   │   │   ├── <span class="morse">····</span> <span class="letter">(H)</span>
      |│   │   │   │   ├── <span class="morse">·····</span> <span class="letter">(5)</span>
      |│   │   │   │   └── <span class="morse">····—</span> <span class="letter">(4)</span>
      |│   │   │   └── <span class="morse">···—</span> <span class="letter">(V)</span>
      |│   │   │       └── <span class="morse">···——</span> <span class="letter">(3)</span>
      |│   │   └── <span class="morse">··—</span> <span class="letter">(U)</span>
      |│   │       ├── <span class="morse">··—·</span> <span class="letter">(F)</span>
      |│   │       └── <span class="morse">··——</span>
      |│   │           └── <span class="morse">··———</span> <span class="letter">(2)</span>
      |│   └── <span class="morse">·—</span> <span class="letter">(A)</span>
      |│       ├── <span class="morse">·—·</span> <span class="letter">(R)</span>
      |│       │   └── <span class="morse">·—··</span> <span class="letter">(L)</span>
      |│       └── <span class="morse">·——</span> <span class="letter">(W)</span>
      |│           ├── <span class="morse">·——·</span> <span class="letter">(P)</span>
      |│           └── <span class="morse">·———</span> <span class="letter">(J)</span>
      |│               └── <span class="morse">·————</span> <span class="letter">(1)</span>
      |└── <span class="morse">—</span> <span class="letter">(T)</span>
      |    ├── <span class="morse">—·</span> <span class="letter">(N)</span>
      |    │   ├── <span class="morse">—··</span> <span class="letter">(D)</span>
      |    │   │   ├── <span class="morse">—···</span> <span class="letter">(B)</span>
      |    │   │   │   └── <span class="morse">—····</span> <span class="letter">(6)</span>
      |    │   │   └── <span class="morse">—··—</span> <span class="letter">(X)</span>
      |    │   └── <span class="morse">—·—</span> <span class="letter">(K)</span>
      |    │       ├── <span class="morse">—·—·</span> <span class="letter">(C)</span>
      |    │       └── <span class="morse">—·——</span> <span class="letter">(Y)</span>
      |    └── <span class="morse">——</span> <span class="letter">(M)</span>
      |        ├── <span class="morse">——·</span> <span class="letter">(G)</span>
      |        │   ├── <span class="morse">——··</span> <span class="letter">(Z)</span>
      |        │   │   └── <span class="morse">——···</span> <span class="letter">(7)</span>
      |        │   └── <span class="morse">——·—</span> <span class="letter">(Q)</span>
      |        └── <span class="morse">———</span> <span class="letter">(O)</span>
      |            ├── <span class="morse">———·</span>
      |            │   └── <span class="morse">———··</span> <span class="letter">(8)</span>
      |            └── <span class="morse">————</span>
      |                ├── <span class="morse">————·</span> <span class="letter">(9)</span>
      |                └── <span class="morse">—————</span> <span class="letter">(0)</span>
      |</div>
      |""".stripMargin
  ).withClass(morseTrie.className)
  .markdownSlides(
    """
      |
      |<div class="tree"><span class="morse">START</span>
      |├── <span class="morse">·</span> <span class="letter">(E)</span>
      |│   ├── <span class="morse">··</span> <span class="letter">(I)</span>
      |│   │   ├── <span class="morse">···</span> <span class="letter">(S)</span>
      |│   │   │   ├── <span class="morse">····</span> <span class="letter">(H)</span>
      |│   │   │   │   ├── <span class="morse">·····</span> <span class="letter">(5)</span>
      |│   │   │   │   └── <span class="morse">····—</span> <span class="letter">(4)</span>
      |│   │   │   └── <span class="morse">···—</span> <span class="letter">(V)</span>
      |│   │   │       └── <span class="morse">···——</span> <span class="letter">(3)</span>
      |│   │   └── <span class="morse">··—</span> <span class="letter">(U)</span>
      |│   │       ├── <span class="morse">··—·</span> <span class="letter">(F)</span>
      |│   │       └── <span class="morse">··——</span>
      |│   │           └── <span class="morse">··———</span> <span class="letter">(2)</span>
      |│   └── <span class="morse">·—</span> <span class="letter">(A)</span>
      |│       ├── <span class="morse">·—·</span> <span class="letter">(R)</span>
      |│       │   └── <span class="morse">·—··</span> <span class="letter">(L)</span>
      |│       └── <span class="morse">·——</span> <span class="letter">(W)</span>
      |│           ├── <span class="morse">·——·</span> <span class="letter">(P)</span>
      |│           └── <span class="morse">·———</span> <span class="letter">(J)</span>
      |│               └── <span class="morse">·————</span> <span class="letter">(1)</span>
      |└── <span class="morse">—</span> <span class="letter">(T)</span>
      |    ├── <span class="morse">—·</span> <span class="letter">(N)</span>
      |    │   ├── <span class="morse">—··</span> <span class="letter">(D)</span>
      |    │   │   ├── <span class="morse">—···</span> <span class="letter">(B)</span>
      |    │   │   │   └── <span class="morse">—····</span> <span class="letter">(6)</span>
      |    │   │   └── <span class="morse">—··—</span> <span class="letter">(X)</span>
      |    │   └── <span class="morse">—·—</span> <span class="letter">(K)</span>
      |    │       ├── <span class="morse">—·—·</span> <span class="letter">(C)</span>
      |    │       └── <span class="morse">—·——</span> <span class="letter">(Y)</span>
      |    └── <span class="morse">——</span> <span class="letter">(M)</span>
      |        ├── <span class="morse">——·</span> <span class="letter">(G)</span>
      |        │   ├── <span class="morse">——··</span> <span class="letter">(Z)</span>
      |        │   │   └── <span class="morse">——···</span> <span class="letter">(7)</span>
      |        │   └── <span class="morse">——·—</span> <span class="letter">(Q)</span>
      |        └── <span class="morse">———</span> <span class="letter">(O)</span>
      |            ├── <span class="morse">———·</span>
      |            │   └── <span class="morse">———··</span> <span class="letter">(8)</span>
      |            └── <span class="morse">————</span>
      |                ├── <span class="morse">————·</span> <span class="letter">(9)</span>
      |                └── <span class="morse">—————</span> <span class="letter">(0)</span>
      |</div>
      |
      |
      |## Predicting what comes next
      |
      |A lot of interesting problems come down to predicting the next step in a sequence: the next
      |card an opponent plays, the next letter in a word, the next position of a moving robot.
      |
      |In principle, the *best* prediction would use everything you've ever observed about that
      |sequence. In practice, that's often too much to track, and most of it barely matters anyway.
      |
      |A **Markov model** makes a bold simplifying assumption instead:
      |
      |> What happens next depends only on the *current* state — not on the full history of how we
      |> got here.
      |
      |That's called the **Markov property**. It's obviously not *exactly* true of most real
      |sequences, but it's often true *enough* to be extremely useful, and it turns "track
      |everything forever" into "just remember where we are right now".
      |
      |""".stripMargin
  )
  .veautifulSlide(<.div(
    <.h2("Let's play Spoof"),
    markdown.div(
      """|**Spoof** is a pub game: each player secretly hides 0–3 coins in a closed fist, then
         |everyone tries to guess the *total* number of coins across every hand.
         |
         |You can never know for certain what a regular opponent is holding — but if they have
         |*tendencies* (rarely repeating their last number, say, or alternating high and low), a
         |Markov model of "what they'll play next, given what they just played" is enough to make a
         |properly informed guess, instead of just assuming every total is equally likely.
         |""".stripMargin
    ),
    <.p(SpoofWidget())
  ))
  .markdownSlides(
    """
      |## Formalising it
      |
      |A (first-order) Markov model has:
      |
      |* A set of possible **states** (here: how many coins someone plays — 0, 1, 2 or 3)
      |* A **transition matrix**: for every state, a probability distribution over what the *next*
      |  state will be
      |
      |That's it. To predict what happens next, look up the row for the *current* state — you don't
      |need to remember anything else about how the sequence got there.
      |
      |Notice in the widget how the predicted distribution for each opponent changes *every round*,
      |simply because their current state (their last play) changed. The model isn't more
      |complicated than that.
      |
      |""".stripMargin
  )
  .markdownSlides(
    """
      |## Sequences of letters are Markov chains too
      |
      |Language has structure a Markov model can pick up on: in English, a **T** is often followed
      |by an **H** (*the*, *this*, *that*), an **H** is often followed by an **E** (*he*, *the*), an
      |**A** is often followed by an **N** (*an*, *and*).
      |
      |The widget below generates a random sequence of letters from a small hand-picked alphabet,
      |one letter at a time, always sampling the next letter from the transition row for whichever
      |letter it's currently on — exactly the same mechanism as the Spoof opponents.
      |
      |Each letter is also shown with a little fixed-length "toy" Morse-style code (three dots/dashes
      |per letter) — we'll be sending these down a noisy line in a minute.
      |
      |""".stripMargin
  )
  .veautifulSlide(<.div(
    <.h2("Generating a letter sequence from a Markov chain"),
    markdown.div(
      """|Click through a few letters and watch the predicted-next-letter chart change — click on a
         |**T** and you should see **H** clearly favoured.
         |""".stripMargin
    ),
    <.p(MorseMarkovWidget())
  ))
  .markdownSlides(
    """
      |## What if we can't see the state directly?
      |
      |So far, every state in our chain has been directly visible: we could see exactly how many
      |coins were played, or exactly which letter came next.
      |
      |But often the state is **hidden**, and all we get is a noisy clue about it. Think of a message
      |sent down a crackly line: dots and dashes occasionally get flipped in transit. The *letters*
      |someone typed are the hidden states; the *corrupted signal* you actually receive is all you
      |observe.
      |
      |That's a **Hidden Markov Model (HMM)**: a Markov chain over hidden states, plus an
      |**emission model** — for each hidden state, a probability distribution over what you might
      |*observe* if the process is actually in that state.
      |
      |""".stripMargin
  )
  .veautifulSlide(<.div(
    <.h2("Decoding a noisy signal"),
    markdown.div(
      """|The **true** row is the real message (normally hidden from a decoder — shown here so you
         |can check the working). The **signal** row is what actually arrived, with flipped symbols
         |highlighted in orange.
         |
         |**Naive** decodes each letter alone, from its own noisy symbols only. **HMM** uses the
         |*Viterbi algorithm* to find the single most likely whole *sequence* of letters — combining
         |the (possibly ambiguous) signal for each letter with how plausible each letter is to follow
         |the one before it.
         |
         |Push the noise slider up and watch Naive's accuracy fall apart faster than the HMM's — the
         |Markov chain's context can out-vote a badly corrupted symbol that the letter-by-letter
         |approach has no way to doubt.
         |""".stripMargin
    ),
    <.p(MorseHmmWidget())
  ))
  .markdownSlides(
    """
      |## The Viterbi algorithm, in a sentence
      |
      |Naively, checking every possible letter sequence for a message of length `n` from an alphabet
      |of size `k` would cost `k^n` — hopelessly expensive.
      |
      |Viterbi instead works one position at a time, and at each position only ever keeps the *single
      |best way to reach each state so far* (throwing away every other, provably worse, way of
      |getting there). That's dynamic programming: the cost drops to `n * k^2`, and it's still
      |guaranteed to find the overall best sequence, not just the best choice at each position taken
      |independently (that's exactly what "Naive" does, and exactly why it can do worse).
      |
      |---
      |
      |## Where you'll meet this again
      |
      |* **Speech recognition** — the classic use case: hidden words/phonemes, noisy audio observations
      |* **Part-of-speech tagging** — hidden grammatical roles, observed words
      |* **Bioinformatics** — hidden gene/protein structure, observed DNA or amino acid sequences
      |* **Spelling & predictive text** — plain Markov chains over letters or words, no hidden state needed
      |
      |Markov models assume the state captures everything relevant about the past — a strong
      |assumption, but if the state is chosen well it's often close enough, and turns a potentially
      |unbounded amount of history into "just remember where we are now".
      |
      |Next: what happens when the hidden state isn't a small handful of discrete values, but a
      |continuous, ever-changing position — like a robot trying to work out where it is?
      |""".stripMargin
  )
  .markdownSlide(willCcBy)
  .renderSlides
