package ai4p.uncertainty
import com.wbillingsley.veautiful.html.*
import com.wbillingsley.veautiful.doctacular.DeckBuilder

import <._
import ^._

import ai4p.{*, given}
import Common.*
import ai4p.uncertainty.widgets.{SpoofWidget, MorseMarkovWidget, MorseHmmWidget, MusicMarkovWidget, VowelConsonantWidget, TextMarkovWidget, HmmDigitWidget}

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
      |# Markov Models 
      |
      |""".stripMargin
  ).withClass("center middle")
  .veautifulSlide(<.div(
    <.h2("Let's make terrible music together"),
    markdown.div(
      """|
         |The widget below generates melodies using a simple "Markov Model"
         |
         |It's pegged to a single scale, with two chord progressions it can alternate between.markovModels
         |
         |All it has is two models going on at once. Each of these is a Markov Model
         |
         |* From the last 1 (or 2) notes played in the melody, pick a next note
         |* From the last chord progression in the base, pick whether to stay in the standard progression or flip to the alternative.
         |
         |""".stripMargin
    ),
    <.p(MusicMarkovWidget())
  ))
  .markdownSlides(
    """
      |## Markov models
      |
      |Markov models are very simple, but they let as start to pivot towards generative AI by thinking about how it's *too simple*
      |
      |A Markov model has:
      |
      |* What token or tokens they've just ouput (their current state) 
      |* A probability table of what they will output next
      |
      |The simplicity means it's easy to see where those probabilities would come from.
      |For example, let's consider Morse code, and whether the next character will be a dot or a dash. 
      |
      |
      |---
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
      |## Originally
      |
      |The first Markov model in 1906 just modelled language as consonants and vowels, and what should come next.
      |
      |""".stripMargin
  )
  .veautifulSlide(<.div(
    <.h2("The very first Markov chain"),
    markdown.div(
      """|In 1913, Andrei Markov took the first 20,000 letters of Pushkin's poem *Eugene Onegin*,
         |and classified them into vowels and consonants. He counted how often a
         |vowel was followed by another vowel versus a consonant (and the same starting from a
         |consonant). Two states, a 2x2 table of probabilities — that's the whole model, and it's
         |where the "Markov chain" gets its name.
         |
         |
         |""".stripMargin
    ),
    <.p(VowelConsonantWidget())
  ))
  .veautifulSlide(<.div(
    <.h2("Generating a letter sequence from a Markov chain"),
    markdown.div(
      """|Let's go *slightly* more complex by considering letters. In English, a **T** is often followed
         |by an **H** (*the*, *this*, *that*), an **H** is often followed by an **E** (*he*, *the*), an
         |**A** is often followed by an **N** (*an*, *and*).
         |
         |Click through a few letters and watch the predicted-next-letter chart change — click on a
         |**T** and you should see **H** clearly favoured.
         |
         |""".stripMargin
    ),
    <.p(MorseMarkovWidget())
  ))
    .markdownSlides(
    """
      |## Text prediction
      |
      |If we extend this to words, we can start doing text prediction. Let's train a Markov model just using the vocabulary in 
      |an out-of-copyright poem.
      |
      |---
      |
      |## Disobedience, by A. A. Milne
      |
      |James James  
      |Morrison Morrison  
      |Weatherby George Dupree  
      |Took great  
      |Care of his Mother,  
      |Though he was only three.  
      |James James Said to his Mother,  
      |"Mother," he said, said he;  
      |"You must never go down  
      |to the end of the town,  
      |if you don't go down with me."  
      |
      |James James  
      |Morrison's Mother  
      |Put on a golden gown.  
      |James James Morrison's Mother  
      |Drove to the end of the town.  
      |James James Morrison's Mother  
      |Said to herself, said she:  
      |"I can get right down  
      |to the end of the town  
      |and be back in time for tea."  
      |
      |King John  
      |Put up a notice,  
      |"LOST or STOLEN or STRAYED!  
      |JAMES JAMES MORRISON'S MOTHER  
      |SEEMS TO HAVE BEEN MISLAID.  
      |LAST SEEN  
      |WANDERING VAGUELY:  
      |QUITE OF HER OWN ACCORD,  
      |SHE TRIED TO GET DOWN  
      |TO THE END OF THE TOWN -  
      |FORTY SHILLINGS REWARD!"  
      |
      |  James James  
      |Morrison Morrison  
      |(Commonly known as Jim)  
      |Told his  
      |Other relations  
      |Not to go blaming him.  
      |James James  
      |Said to his Mother,  
      |"Mother," he said, said he:  
      |"You must never go down to the end of the town  
      |without consulting me."  
      |
      |James James  
      |Morrison's mother  
      |Hasn't been heard of since.  
      |King John said he was sorry,  
      |So did the Queen and Prince.  
      |King John  
      |(Somebody told me)  
      |Said to a man he knew:  
      |If people go down to the end of the town, well,  
      |what can anyone do?"  
      |
      |""".stripMargin
  )
  .veautifulSlide(<.div(
    <.h2("Word-by-word prediction"),
    markdown.div(
      """|The widget below is a Markov model whose *entire* vocabulary and transition table come
         |from whatever text sits in the left-hand box. (You can replace the text to re-train it)
         |
         |Each line ends with an end-of-line token (shown as ⏎), to illustrate how we can have non-work tokens
         |
         |""".stripMargin
    ),
    <.p(TextMarkovWidget())
  ))
  .veautifulSlide(<.div(
    <.h2("Let's play Spoof"),
    markdown.div(
      """|**Spoof** is a pub game: each player secretly hides 0–3 coins in a closed fist, then
         |everyone tries to guess the *total* number of coins across every hand.
         |
         |This model supposes that we've seen these two players play fairly often, and can train a model
         |based on their particular personal tendencies in how they play.
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
      |* A set of possible **states** (e.g., vowel/consonant; or letters; or notes; or words; or number of coins played)
      |* A **transition matrix**: for every state, a probability distribution for what the *next* state will be
      |
      |Markov Models only use the current state, though we can decide that the state should contain more than one token of history
      |
      |""".stripMargin
  )
  .markdownSlides(
    """
      |## Hidden Markov Models (HMMS) - What if we can't see the state?
      |
      |So far, every state in our chain has been directly visible: we could see exactly how many
      |coins were played, or exactly which letter came next.
      |
      |Sometimes, we can't see the real state, only some outward behaviour. 
      |
      |That might be because of noisy transmission of data over the radio, or it could be because the
      |real state just isn't visible. 
      |
      |For these, we use a "Hidden Markov Model" (HMM), where we use the observed behaviour to deduce
      |what the *most likely* sequence of states was.
      |
      |""".stripMargin
  )
  .veautifulSlide(<.div(
    <.h2("Decoding a noisy signal"),
    markdown.div(
      """|The **true** row is the real message  we were trying to send. 
         |
         |The **signal** row is what actually arrived, with flipped symbols highlighted in orange.
         |
         |**Naive** decodes each letter alone, from its own noisy symbols only. **HMM** uses the
         |*Viterbi algorithm* to find the single most likely whole *sequence* of letters — combining
         |the (possibly ambiguous) signal for each letter with how plausible each letter is to follow
         |the one before it.
         |
         |""".stripMargin
    ),
    <.p(MorseHmmWidget())
  ))
  .veautifulSlide(<.div(
    <.h2("A second example: recognising a hand-drawn digit"),
    markdown.div(
      """|The same idea works well beyond text. Here, the "signal" is a hand-drawn stroke: it gets
         |turned into a sequence of compass-direction observations (a *chain code*), and each digit
         |0-9 has its own small left-to-right HMM whose hidden states are "which segment of this
         |digit's shape am I currently drawing".
         |
         |Viterbi decoding against all ten digit-models both scores which digit best explains your
         |strokes, and — because it recovers the whole hidden *state path*, not just a score — shows
         |exactly which of your strokes it thinks corresponds to which segment of the winning digit.
         |""".stripMargin
    ),
    <.p(HmmDigitWidget())
  ))
  .markdownSlides(
    """
      |## Some limitations
      |
      |Markov Models aren't an LLM but we can get a sense of the idea of generating "likely" content.
      |
      |But here's some limitations that the more complex AI models help us overcome:
      |
      |* Much longer memory - we've been working with just 1 or 2 recent tokens, rather than thousands
      |
      |* Understanding context and complex relationships between language elements
      |
      |* Understanding that words have multiple meanings. In *Disobedience*, the word "he" refers to James James in one line, but King John in another
      |
      |* The ability to work out what's a likely next token for a prefix it *hasn't* seen before. 
      |
      |For those, we'll need much more complex machinery
      |
      |""".stripMargin
  )
  .markdownSlide(willCcBy)
  .renderSlides
