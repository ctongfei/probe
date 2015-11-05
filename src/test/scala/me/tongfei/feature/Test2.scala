package me.tongfei.feature

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object Test2 extends App {

  type Sentence = Seq[String]

  val WordFeaturizer = Featurizer("word") { (s: Sentence) =>
    BinaryFeatureGroup("word")(s)
  }

  val Letter3GramFeaturizer = Featurizer("3gram") { (s: String) =>
    BinaryFeatureGroup("3gram")(s.sliding(3).map(_.mkString).toIterable)
  }

  val top6_3grams = (WordFeaturizer >>> Letter3GramFeaturizer).topK(6)
  val top3_words = WordFeaturizer.topK(3)

  val s = "John killed Mary who killed James who killed Hillary".split(" ")
  val fx = FeaturizerSet(top3_words, top6_3grams)
  val fvs = fx(s).toStringFeatureVector


  val bp = 0

}
