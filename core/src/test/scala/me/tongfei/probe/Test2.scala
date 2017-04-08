package edu.jhu.hlt.probe

import scala.collection.JavaConversions._
/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object Test2 extends App {

  type Sentence = Seq[String]

  val WordFeaturizer = Featurizer.count("word") { (s: Sentence) => s }

  val Letter3GramFeaturizer = Featurizer.count("3gram") { (s: String) => s.sliding(3).map(_.mkString).toIterable }

  val top6_3grams = (WordFeaturizer >>> Letter3GramFeaturizer).topK(6)

  val taggedTop6_3grams = top6_3grams tagWith { a: Sentence => a.length }

  val top3_words = WordFeaturizer.topK(3)

  val s1 = "John killed Mary who killed James who killed Hillary".split(" ").toSeq
  val s2 = "John saved Mary who saved Lily who saved Henry".split(" ").toSeq

  val t = taggedTop6_3grams(s1)

  val fx = top3_words ++ top6_3grams
  val fx1 = top3_words

  val singleFeature = SingleNumericalFeature("a")(2)

  implicit val context: Any = ()

  val ff = CosineSimilarity(top3_words, top3_words)
  val ff12 = ff((s1, s2))
  val fvs1 = FeatureVector.from(fx.extract(s1))
  val fvs2 = FeatureVector.from(fx.extract(s2))

  val s = fvs1.toString
  val fvs1restored = FeatureVector.parse(s)

  val j = fvs1 + fvs2
  val j2 = -fvs1
  val j3 = fvs1 - fvs2
  val j4 = fvs1 * 0.5

  val fcos = CosineSimilarity(top3_words, top3_words)
  val jcos5 = fcos((s1, s2))

  val j5 = FeatureVector(fx1(s1)) cosSimilarity FeatureVector(fx1(s2))

  val bp = 0

}
