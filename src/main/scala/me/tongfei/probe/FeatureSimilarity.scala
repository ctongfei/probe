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
    Featurizer.singleNumerical(s"$similarityName(${f1.name},${f2.name})") { (pair: (A, B)) =>
      val (a, b) = pair
      val fa = f1.extract(a)
      val fb = f2.extract(b)
      self.apply(fa, fb)
    }
  }

  def apply[A, B](f: Featurizer[A, B]): Featurizer[(A, A), Unit] = apply(f, f)
}



object FeatureSimilarity {

  def zipKey[C](fa: FeatureGroup[C], fb: FeatureGroup[C]): Iterable[(C, (Double, Double))] = {
    val mb = fb.pairs.toMap
    for {
      (ka, va) ← fa.pairs if mb contains ka
    } yield ka → (va, mb(ka))
  }

}
