package edu.jhu.hlt.probe

import edu.jhu.hlt.granite._
import edu.jhu.hlt.granite.io._

/**
 * @author Tongfei Chen
 */
object FeaturizerFamilyTest extends App {

  val comm = ConcreteIO.load("/Users/tongfei/my/data/LDC2014E81/annotated/AFP_ENG_20090605.0363.comm")

  val NE = FeaturizerFamily.binary("ne") { s: Sentence =>
    for {
      ner <- s.tokenization.nerTagging.toList
      t <- ner.taggedTokenList if (t.tag != "O")
    } yield t.tag -> s.tokenization.tokenList(t.tokenIndex).text
  }

  val s = comm.sectionList(0).sentenceList(0)
  val t = comm.sectionList(1).sentenceList(0)

  val fvx = Dot(NE, NE)

  val fv = FeatureVector.from(NE(s))

  val fvxx = FeatureVector.from(fvx(s, s))

  val bp = 0
}
