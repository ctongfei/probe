package me.tongfei.probe

/**
  * Represents a similarity function that when given two feature groups,
  * produces in a single similarity feature.
  * @author Tongfei Chen (ctongfei@gmail.com).
  * @since 0.4.1
  */
trait FeatureSimilarity { self =>

  def similarityName: String

  /** Given two feature groups, computes their similarity measure. */
  //TODO: should C be modeled as an existential type?
  def apply[C](fa: FeatureGroup[C], fb: FeatureGroup[C]): Double

  /** Given two featurizers, produces a featurizer that returns a single feature that contains their similarity. */
  def apply[A, B, C](f1: Featurizer[A, C], f2: Featurizer[B, C]): Featurizer[(A, B), Unit] = {
    new Featurizer[(A, B), Unit] {
      def name = s"$similarityName(${f1.name},${f2.name})"
      def apply(pair: (A, B)) = {
        val (a, b) = pair
        val fa = f1(a)
        val fb = f2(b)
        FeatureGroup.fast(name)(Iterable(() → self.apply(fa, fb)))
      }
    }
  }
}



object FeatureSimilarity {

  def zipKey[C](fa: FeatureGroup[C], fb: FeatureGroup[C]): Iterable[(C, (Double, Double))] = {
    val mb = fb.pairs.toMap
    for {
      (ka, va) ← fa.pairs if mb contains ka
    } yield ka → (va, mb(ka))
  }

}
