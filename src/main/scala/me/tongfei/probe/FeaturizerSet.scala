package me.tongfei.probe

/**
  * Represents a set of featurizers.
  * @author Tongfei Chen (ctongfei@gmail.com).
  * @since 0.4.0
  */
sealed trait FeaturizerSet[-A, B] extends (A => Iterable[FeatureGroup[B]]) { self =>

  def applyOnGroup[A1 <: A](ga: FeatureGroup[A1]): Iterable[FeatureGroup[B]]

  def apply(a: A): Iterable[FeatureGroup[B]]

  def >>>[C](that: FeaturizerSet[B, C]): FeaturizerSet[A, C] = new ComposedFeaturizerSet(self, that)

}

case class SimpleFeaturizerSet[-A, B](featurizers: Iterable[Featurizer[A, B]]) extends FeaturizerSet[A, B] {
  def apply(a: A) = for (f ← featurizers) yield f(a)

  def applyOnGroup[A1 <: A](ga: FeatureGroup[A1]) =
    for (f ← featurizers) yield ga flatMap f

  def ++[A1 <: A](f: Featurizer[A1, B]) = SimpleFeaturizerSet(featurizers ++ Iterable(f))
}

case class ComposedFeaturizerSet[-A, B, C](f1: FeaturizerSet[A, B], f2: FeaturizerSet[B, C]) extends FeaturizerSet[A, C] {

  def applyOnGroup[A1 <: A](ga: FeatureGroup[A1]): Iterable[FeatureGroup[C]] = {
    val f1a = f1.applyOnGroup(ga)
    for {
      fg1 ← f1a
      fg2 ← f2.applyOnGroup(fg1)
    } yield fg2
  }

  def apply(a: A) = {
    val f1a = f1(a)
    for {
      fg1 ← f1a
      fg2 ← f2.applyOnGroup(fg1)
    } yield fg2
  }
}

