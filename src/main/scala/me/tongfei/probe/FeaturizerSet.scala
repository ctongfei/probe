package me.tongfei.probe

/**
  * Represents a set of featurizers.
  * @author Tongfei Chen (ctongfei@gmail.com).
  * @since 0.4.0
  */
trait FeaturizerSet[-A, +B] extends (A => Iterable[FeatureGroup[B]]) { self =>

  def featurizers: Iterable[Featurizer[A, B]]

  def apply(a: A) = featurizers.map(f => f(a))

  def >>>[C](that: FeaturizerSet[B, C]): FeaturizerSet[A, C] = new FeaturizerSet[A, C] {
    def featurizers = for {
      f1 ← self.featurizers
      f2 ← that.featurizers
    } yield f1 >>> f2

    override def apply(a: A) = {
      val f1a = self(a)
      for {
        fg1 ← f1a
        f2 ← that.featurizers
      } yield f2.applyOnGroup(fg1)
    }
  }
}

object FeaturizerSet {

  def apply[A, B](fs: Featurizer[A, B]*): FeaturizerSet[A, B] = new FeaturizerSet[A, B] {
    def featurizers = fs
  }

}
