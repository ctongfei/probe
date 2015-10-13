package me.tongfei.feature.pipe

import poly.algebra.hkt.ops._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object Test extends App {

  type Sentence = Seq[String]

  object WordFeaturizer extends Featurizer[Sentence, String] {
    def apply(s: Sentence) = FeatureGroup("word")(s)
  }

  object Letter3GramFeaturizer extends Featurizer[String, String] {
    def apply(s: String) = FeatureGroup("3gram")(s.sliding(3).map(_.mkString).toIterable)
  }

  val fx = WordFeaturizer >>> Letter3GramFeaturizer

  val s = "John killed Mary".split(" ")
  val f = fx(s)

  val bp = 0

}
