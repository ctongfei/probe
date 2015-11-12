package me.tongfei.probe

/**
  * Represents a feature extractor, which contains a list of featurizers.
  * @author Tongfei Chen (ctongfei@gmail.com).
  * @since 0.5.0
  */
sealed trait FeatureExtractor[-A, B] extends (A => Iterable[FeatureGroup[B]]) { self =>

  /**
    * Applies this feature extractor on every key in a feature group.
    */
  def applyOnGroup[A1 <: A](ga: FeatureGroup[A1]): Iterable[FeatureGroup[B]]

  /**
    * Applies this feature vector on an element.
    * @return A list of feature groups extracted out of this feature extractor.
    */
  def apply(a: A): Iterable[FeatureGroup[B]]

  /**
    * Composes two feature extractors by first piping an object through the first extractor and then the second.
    * @return A composed feature extractor.
    */
  def >>>[C](that: FeatureExtractor[B, C]): FeatureExtractor[A, C] = new ComposedFeatureExtractor(self, that)

  /**
    * Concatenates two feature extractors.
    * @return A concatenated feature extractor.
    */
  def ++[A1 <: A](that: FeatureExtractor[A1, B]): FeatureExtractor[A1, B] = new ConcatenatedFeatureExtractor(Iterable(self, that))

}

case class TrivialFeatureExtractor[-A, B](featurizer: Featurizer[A, B]) extends FeatureExtractor[A, B] {
  def applyOnGroup[A1 <: A](ga: FeatureGroup[A1]) = Iterable(ga flatMap featurizer)
  def apply(a: A) = Iterable(featurizer(a))
}

case class ConcatenatedFeatureExtractor[-A, B](featurizers: Iterable[FeatureExtractor[A, B]]) extends FeatureExtractor[A, B] {
  def apply(a: A) = for {
    f ← featurizers
    fga ← f(a)
  } yield fga

  def applyOnGroup[A1 <: A](ga: FeatureGroup[A1]) = for {
    f ← featurizers
    fga ← f.applyOnGroup(ga)
  } yield fga

  override def ++[A1 <: A](f: FeatureExtractor[A1, B]) = f match {
    case ConcatenatedFeatureExtractor(g) => ConcatenatedFeatureExtractor(featurizers ++ g)
    case _ => ConcatenatedFeatureExtractor(featurizers ++ Iterable(f))
  }
}

case class ComposedFeatureExtractor[-A, B, C](f1: FeatureExtractor[A, B], f2: FeatureExtractor[B, C]) extends FeatureExtractor[A, C] {

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

