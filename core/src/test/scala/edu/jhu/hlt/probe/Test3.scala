package edu.jhu.hlt.probe

/**
  * @author Tongfei Chen (ctongfei@gmail.com).
  */
object Test3 extends App {

  val f1 = DenseVectorFeature("emb")(Array(1.0, 2.0, 3.0))
  val f2 = DenseVectorFeature("emb2")(Array(3.0, 2.0, 1.0))

  val f3 = SingleNumericalFeature("cos(emb)")(CosineSimilarity.apply(f1, f2))
  val f4 = SingleNumericalFeature("jaccard(emb)")(JaccardSimilarity.apply(f1, f2))

  val f = FeatureVector(f3, f4)
  val fs = f.toString
  val fr = FeatureVector.parse(fs)

  val bp = 0
}
