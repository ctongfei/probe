package me.tongfei.probe

/**
  * @author Tongfei Chen (ctongfei@gmail.com).
  */
object Test3 extends App {

  val f1 = DenseVectorFeature("emb")(Array(1.0, 2.0, 3.0))
  val f2 = DenseVectorFeature("emb2")(Array(3.0, 2.0, 1.0))

  val f3 = SingleFeature("cos(emb)")(FeatureSimilarity.Cosine.similarity(f1, f2))

  val bp = 0
}
