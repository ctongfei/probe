package me.tongfei.feature

/**
  * Represents a set of featurizers.
  * @author Tongfei Chen (ctongfei@gmail.com).
  * @since 0.4.0
  */
trait FeaturizerSet[A] extends (A => FeatureVector) {

  def featurizers: Seq[Featurizer[A, Any]]

  def apply(a: A) = FeatureVector(featurizers.map(f => f(a)): _*)
}

object FeaturizerSet {

  def apply[A](fs: Featurizer[A, Any]*): FeaturizerSet[A] = new FeaturizerSet[A] {
    def featurizers = fs
  }

}
