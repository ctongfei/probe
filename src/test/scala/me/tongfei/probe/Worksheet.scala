package me.tongfei.probe
import me.tongfei.granite._
import me.tongfei.granite.io._

/**
 * @author Tongfei Chen
 */
object Worksheet extends App {

  val comm = ConcreteIO.load("/Users/tongfei/my/data/LDC2014E81/annotated/AFP_ENG_20090605.0363.comm")

  val s = comm.sectionList(0).sentenceList(0)


  val sentenceString = comm.text.substring(s.textSpan.start, s.textSpan.ending)

  val BagOfWords = Featurizer.binary("word") { s: Sentence =>
    s.tokenization
      .tokenTaggingList.find(_.taggingType == "LEMMA").get
      .taggedTokenList.map(_.tag)
  }

  println(BagOfWords(s))

  val LowerCasedBagOfWords = BagOfWords.map(_.toLowerCase)

  println(LowerCasedBagOfWords(s))

  val NamedEntityTypes = Featurizer.binary("NEType") { s: Sentence =>
    for {
      ner <- s.tokenization.tokenTaggingList.find(_.taggingType == "NER").toList
      tok <- ner.taggedTokenList if tok.tag != "O"
    } yield tok.tag
  }

  //println(NamedEntityTypes(s))

  val Root = Featurizer.singleCategorical("root") { s: Sentence =>
    val i = s.tokenization.dependencyParseList(0).dependencyList.find(_.gov == -1).get.dep
    s.tokenization.tokenList(i).text
  }

  println(Root(s))

  val bp = 0

}
