package me.tongfei.probe

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object Test2 extends App {

  type Sentence = Seq[String]

  val WordFeaturizer = Featurizer("word") { (s: Sentence) =>
    FeatureGroup.count("word")(s)
  }

  val Letter3GramFeaturizer = Featurizer("3gram") { (s: String) =>
    FeatureGroup.count("3gram")(s.sliding(3).map(_.mkString).toIterable)
  }

  val top6_3grams = (WordFeaturizer >>> Letter3GramFeaturizer).topK(6)
  val top3_words = WordFeaturizer.topK(3)

  val s1 = "John killed Mary who killed James who killed Hillary".split(" ")
  val s2 = "John saved Mary who saved Lily who saved Henry".split(" ")

  val t = top6_3grams(s1)

  val fx = FeaturizerSet(top3_words, top6_3grams)
  val fx1 = FeaturizerSet(top3_words)

  val singleFeature = SingleFeature("a")(2)

  val ff = FeaturizerSet(FeatureSimilarity.Cosine(top3_words, top3_words))
  val ff12 = ff(s1, s2)
  val fvs1 = fx(s1)
  val fvs2 = fx(s2)

  val s = fvs1.toString
  val fvs1restored = FeatureVector.parse(s)

  val j = fvs1 + fvs2
  val j2 = -fvs1
  val j3 = fvs1 - fvs2
  val j4 = fvs1 * 0.5

  val fcos = FeatureSimilarity.Cosine(top3_words, top3_words)
  val jcos5 = fcos(s1, s2)

  val j5 = fx1(s1) cosSimilarity fx1(s2)

  val bp = 0

}
