package me.tongfei.feature

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object Test2 extends App {

  type Sentence = Seq[String]

  object WordFeaturizer extends Featurizer[Sentence, String] {
    def apply(s: Sentence) = FeatureGroup("word")(s)
  }

  object Letter3GramFeaturizer extends Featurizer[String, String] {
    def apply(s: String) = FeatureGroup("3gram")(s.sliding(3).map(_.mkString).toIterable)
  }

  val fx = (WordFeaturizer >>> Letter3GramFeaturizer).topK(6)

  val s = "John killed Mary who killed James who killed Hillary".split(" ")
  val fs = fx(s)

  val bp = 0

}
