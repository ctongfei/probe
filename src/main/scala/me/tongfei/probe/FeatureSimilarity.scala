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
  def apply[C](fa: FeatureGroup[C], fb: FeatureGroup[C]): Double

  /** Given two featurizers, produces a featurizer that returns a single feature that contains their similarity. */
  def apply[A, B, C](f1: Featurizer[A, C], f2: Featurizer[B, C]): Featurizer[(A, B), Unit] =
    new FeatureSimilarityT.SimilarityFeaturizer(self, f1, f2)


  def apply[A, B](f: Featurizer[A, B]): Featurizer[(A, A), Unit] = apply(f, f)

  def apply[A1, A2, B, C1, C2](f1: ContextualizedFeaturizer[A1, B, C1], f2: ContextualizedFeaturizer[A2, B, C2]): ContextualizedFeaturizer[(A1, A2), Unit, (C1, C2)] = {
    ContextualizedFeaturizer.singleNumerical(s"$similarityName(${f1.name},${f2.name})") { (pair: (A1, A2), c: (C1, C2)) =>
      val (a, b) = pair
      val (ca, cb) = c
      val fa = f1.extract(a, ca)
      val fb = f2.extract(b, cb)
      self.apply(fa, fb)
    }
  }

  def apply[A, B, C](f: ContextualizedFeaturizer[A, B, C]): ContextualizedFeaturizer[(A, A), Unit, (C, C)] = apply(f, f)
}



object FeatureSimilarity {

  def zipKey[C](fa: FeatureGroup[C], fb: FeatureGroup[C]): Iterable[(C, (Double, Double))] = {
    val mb = fb.pairs.toMap
    for {
      (ka, va) ← fa.pairs if mb contains ka
    } yield ka → (va, mb(ka))
  }

}

private[tongfei] object FeatureSimilarityT {

  case class SimilarityFeaturizer[A, B, C](sim: FeatureSimilarity, f1: Featurizer[A, C], f2: Featurizer[B, C]) extends Featurizer[(A, B), Unit] {
    val name = s"${sim.similarityName}(${f1.name},${f2.name})"
    def extract(pair: (A, B)) = {
      val (a, b) = pair
      val fa = f1.extract(a)
      val fb = f2.extract(b)
      SingleNumericalFeature(name)(sim(fa, fb))
    }
  }

}