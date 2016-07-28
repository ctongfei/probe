package me.tongfei.probe

import me.tongfei.granite._
import me.tongfei.granite.io._

/**
 * @author Tongfei Chen
 */
object FeaturizerFamilyTest extends App {

  val comm = ConcreteIO.load("/Users/tongfei/my/data/LDC2014E81/annotated/AFP_ENG_20090605.0363.comm")

  val NE = FeaturizerFamily.binary("ne") { s: Sentence =>
    for {
      ner <- s.tokenization.nerTagging.toList
      t <- ner.taggedTokenList
    } yield t.tag -> s.tokenization.tokenList(t.tokenIndex).text
  }

  val s = comm.sectionList(0).sentenceList(0)

  val fv = FeatureVector.from(NE(s))

  val bp = 0
}
