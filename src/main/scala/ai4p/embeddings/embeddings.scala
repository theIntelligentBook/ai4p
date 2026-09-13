package ai4p.embeddings

import com.wbillingsley.veautiful.html.*
import com.wbillingsley.veautiful.doctacular.DeckBuilder

import <._
import ^._

import ai4p.{*, given}
import Common.*
import ai4p.neuralnets.widgets.NetworkView

import site.given

import ai4p.embeddings.widgets.*


val embeddings = DeckBuilder(1920, 1080)
  .markdownSlide(
    """
      |# Embeddings and Transformers
      |
      |""".stripMargin
  ).withClass("center middle")
  .veautifulSlide(<.div(
    markdown.div("""|## The Tale of Peter Rabbit
                    |
                    |Below, I've put a widget showing the text of Peter Rabbit by Beatrix Potter, 
                    |and if you click on a word in the widget, it'll highlight which other lines contain that word.
                    |
                    |Alongside it, it shows this as a "vector" of zeroes and 1s.
                    |""".stripMargin),
    <.p(PeterRabbitWidget()),
    <.p("The point here is just that we can take a word and turn it into some sort of vector, based on how it's used.")
  ))
  .veautifulSlide(<.div(
    markdown.div("""|## You shall know a word by the company it keeps
                    |
                    |That's a quite from linguist J.R. Firth from 1957, but we can use it to assess the relatedness of words in our corpus
                    |
                    |""".stripMargin),
    <.p(LineVectorWidget()),
  ))
  .markdownSlides(
    """
      |## But...
      |
      |* This tells us when words co-occur, but not when they are *used similarly* with other words. 
      |  Despite **cat** and **mouse** both being animals Peter meets alone, on his own, **mouse** shows as having nothing in common with **cat**. 
      |
      |* Every new line of text we add to a corpus (e.g. another story) woudl add another dimension to every word's vector.
      |
      |So, we have a concept that we can turn words into vectors of numbers based on how they are used, but this isn't really a good way of doing it.
      |
      |""".stripMargin
  )
  .veautifulSlide(<.div(
    markdown.div(
      """|## Word2Vec (2013)
         |
         |Word2Vec tried instead to *learn* a vector representation of each word.
         |Train a neural network with the task of predicting the words either side of it.
         |
         |""".stripMargin
    ),
    <.p(NetworkView.render(NetworkView.denseDiagram(
      layerSizes = Vector(6, 3, 6),
      weight = (li, from, to) => math.sin(from * 1.3 + to * 0.9 + li),
      layerLabels = Vector("Word", "hidden layer", "Predicted neighbour"),
      width = 480, height = 240
    ))),
    markdown.div(
      """|Once it's trained, the values output by the hidden layer become our vector representation of the word.
         |
         |It has however many dimensions we chose (how many neurons we put in the hidden layer) and learns as dense a representation of the word as possible.
         |
         |This kind of vector representation of a word is called an **embedding**.
         |""".stripMargin
    )
  ))
  .veautifulSlide(<.div(
    markdown.div("""|## A too-little one
                  |
                  |Peter Rabbit is not enough text, but we can go through the motions and train a neural network to give each word an embedding.
                  |
                  |This is set up with just 2 nodes in the hidden layer, just so we can plot the learned embeddings as dots on a screen.
                  |
                  |It's not useful at this scale, but hopefully illustrates the process.
                  |
                  |""".stripMargin),
    <.p(Word2VecWidget())
  ))
  .markdownSlides(
    """
      |## Here's one someone else made earlier...
      |
      |Training word2vec properly needs a huge amount of text. 
      |
      |The widget on the next slide loads real **GloVe** vectors (Pennington, Socher & Manning, 2014) -
      |trained the same "learn from context" way, just via a slightly different mechanism to word2vec -
      |trimmed down to the ~10,000 most common words so it's light enough to load straight into the page.
      |
      |""".stripMargin
  )
  .veautifulSlide(<.div(
    markdown.div("""|## Here's one someone else made earlier
                    |
                    |Training a model would require a lot of data. So let's use one someone else made.
                    |
                    |GloVe "Global Vectors for Word Representation" wasn't trained using a neural network, but a simpler regression model
                    |that tries to make the dot-product (similarity) of two word's vectors match the log of their co-occurrence count in a large corpus.
                    |
                    |But it has the same effect: it learns a 50-dimension vector to represent a word.
                    |
                    |https://nlp.stanford.edu/projects/glove/
                    |
                    |""".stripMargin),
    <.p(NearestNeighboursWidget())
  ))
  .veautifulSlide(<.div(
    markdown.div(
      """|## Vector algebra
         |
         |If an embedding is a vector, then what happens if we add or subtract them?
         |
         |Sometimes, the learned vectors seem to line up so that you can get equations that make sense. 
         |e.g. **king - man + woman ≈ queen**. 
         |
         |That's not an intentional effect, it's just that there are a lot of examples in the corpus of
         |gender-swapped texts that the relatonship emerges.
         |
         |However, notice that "Paris - France + Germany" works, but "Paris - France + England" goes slightly astray.
         |
         |""".stripMargin
    ),
    <.p(WordAlgebraWidget())
  ))
  .veautifulSlide(<.div(
    <.h2("The same offset, in two different pairs of words"),
    markdown.div("Real 50-d vectors, projected onto the two directions that separate these pairs best:"),
    <.p(AnalogyDiagram().render)
  ))
  .markdownSlides(
    """
      |## Words have a lot of dimensions
      |
      |Let's think about the word "king"
      |
      |It could have a lot of conceptual characteristics:
      |
      |* Male
      |* Ruler
      |* Hereditary
      |
      |But it also has linguistic aspects to it, e.g.:
      |
      |* it's a noun
      |* often precedes a name (e.g. King Charles)
      |* sometimes a surname (e.g. Elektra King)
      |* or a card (e.g. King of Hearts)
      |
      |If we want an AI to understand *context*, there are a lot of possible dimensions of relatedness that it might need to consider
      |""".stripMargin
  )
  .markdownSlides(
    """
      |## Attention
      |
      |When we look at a word, we're going to:
      |
      |* ask "what other words are relevant to me right now?" (what should we pay attention to)
      |* blend in a bit of their word vectors to produce a new vector
      |
      |Which aspect of "king" should we pay attention to though? It being a noun? It being royal? It being male?
      |
      |We're just going to have lots of "attention heads" and let them *learn* to pay attention to different stuff.
      |
      |* In one attention head, it might pull in a bit of "male"
      |* In another it might pull in a bit of "Hearts", "Clubs", "Diamonds", and "Spades"
      |* In another, it might pull in a bit of all the other nouns
      |* in another, it might pull in a bit of common male names
      |
      |i.e. we're going to let the network figure out what it even means to be related to other words
      |
      |""".stripMargin
  )
  .veautifulSlide(<.div(
    markdown.div("""|## A quick attention-like visualisation
                    |
                    |These text fragments are tiny, but hopefully this gives the idea that when you select a token
                    |it pays "attention" to some amount of some other words in the input
                    |
                    |""".stripMargin),
    <.p(AttentionWidget())
  ))
  .markdownSlides(
    """
      |## How attention works
      |
      |Each attention head learns three "matrices" (grids of numbers to transform emeddings by):
      |
      |* W<sub>Q</sub>, which will produce **queries** - multiply it by a word's embedding and you get the word's "query" vector
      |* W<sub>K</sub>, which will produce **keys** - multiply it by a word's embedding and you get the word's "key" vector
      |* W<sub>V</sub>, which will produce **values** - multiply it by a word's embedding and you get the word's "value" vector
      |
      |Suppose we are looking at a token A, and we want to know it's relationship with token B. 
      |
      |The similarity is taken from word A's query and word B's key.
      |
      |* AW<sub>Q</sub> ⋅ BW<sub>K</sub>
      |
      |That much of word B's value will then get combined with word A's value.
      |
      |By learning different W<sub>Q</sub> and W<sub>K</sub> matrices, each attention head learns to consider a different kind of relationship between words
      |(grammar, reference, topic, ...) and having a different W<sub>V</sub> matrix then lets them output a different result.
      |
      |---
      |
      |## When we say **Large** Language Model...
      |
      |BERT (2018) had 12 attention heads in each layer, and 768 dimensions (rows) in an embedding vector for a word/token. 
      |
      |GPT-4 (2023) had 96 attention heads in each layer, and 3,072 dimensions (rows) in an embedding vector for a word/token.
      |
      |The vectors and matrices are much too big to visualise here.
      |
      |""".stripMargin
  )
  .veautifulSlide(<.div(
    markdown.div(
      """|## Why a "transformer" model?
         |
         |Originally, transformer models were designed to transform text, e.g. from French to English.
         |
         |However, the name also makes sense if you look at those operations of multiplying matrices by embeddings -
         |a matrix "transforms" a vector into a different vector space.
         |
         |
         |""".stripMargin
    ),
    <.p(AffineTransformWidget())
  ))
  .veautifulSlide(<.div(
    <.h2("One transformer layer"),
    <.p(TransformerBlockDiagram.render),
    <.p("The output of this layer then becomes the input for the next layer. GPT-4 had 96 layers. i.e. 96 of these connected together")
  ))
  .markdownSlides(
    """
      |## Visualising a larger network
      |
      |https://poloclub.github.io/transformer-explainer/
      |
      |""".stripMargin
  )
  .veautifulSlide(<.div(
    <.h2("It's word prediction, but..."),
    <.p(RelationshipRepeaterWidget()),    
  ))
  .markdownSlides(
      """|## The story so far
         |
         |Back in the neural networks deck, we mentioned recurrent networks add loops to handle sequences
         |over time... but that **LLMs don't do this**. Now you can see why they don't need to:
         |
         |* **Embeddings** turn each token into a vector that starts out capturing *what the word means*
         |* **Attention** lets every token update itself using every *other* token in the sequence, all at
         |  once (no need to step through it one word at a time)
         |* The **feed-forward** layer in each block (built from the same SiLU/SwiGLU-style neurons from
         |  the neural networks deck) then reworks each token's vector on its own
         |
         |Stack a few dozen of these blocks, train on enough text, and you get a large language model.
         |
         |---
         |
         |## So what?
         |
         |* LLMs can learn very complex multi-dimensional relationships between text
         |
         |* They don't really look much like a human brain though. They look far more like learned matrix transformations feeding very deeply into each other, with enormous memories.
         |
         |* Going 96 layers deep, they can capture very high-order relationships between text.
         |
         |* They are still remixing text, to predict the next output token, but in very complex ways.
         |
         |**Everything** in the LLM is trained. The embeddings (how to organise what things mean). The weight matrices in every attention head in every layer (what to pay attention to). 
         |The feed-forward neural network between layers (how to transform the result into a higher order representation)
         |
         |You could say LLMs are copy-cats and launderers of other people's ideas (what it's learned from their text, remixed through the layers of the network). 
         |But they are fantastically subtle copy-cats with enormous memories that can remix the ideas
         |of more text than you could read in a lifetime.
         |
         |""".stripMargin
    )
  .markdownSlide(willCcBy)
  .renderSlides
