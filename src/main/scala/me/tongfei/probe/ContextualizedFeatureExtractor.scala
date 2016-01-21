package me.tongfei.probe

/**
  * Represents a contextualized feature extractor, which contains a list of contextualized featurizers.
  * @author Tongfei Chen (ctongfei@gmail.com).
  * @since 0.6.0
  */
sealed trait ContextualizedFeatureExtractor[-X, Y, C] { self =>

  import ContextualizedFeatureExtractor._

  /**
    * Applies this feature extractor on every key in a feature group.
    */
  def applyOnGroup[X1 <: X](ga: FeatureGroup[X1])(implicit c: C): Iterable[FeatureGroup[Y]]

  /**
    * Applies this feature vector on an element.
    * @return A list of feature groups extracted out of this input object.
    */
  def apply(a: X)(implicit c: C): Iterable[FeatureGroup[Y]]

  /**
    * Composes two feature extractors by first piping an object through the first extractor and then the second.
    * @return A composed feature extractor.
    */
  def >>>[Z, C1 >: C](that: ContextualizedFeatureExtractor[Y, Z, C1]): ContextualizedFeatureExtractor[X, Z, C]
    = new Composed(self, that)

  /**
    * Concatenates two feature extractors.
    * @return A concatenated feature extractor.
    */
  def ++[X1 <: X](that: ContextualizedFeatureExtractor[X1, Y, C]): ContextualizedFeatureExtractor[X1, Y, C]
    = new Concatenated(Iterable(self, that))

}

object ContextualizedFeatureExtractor {

  case class Trivial[-X, Y, C](featurizer: ContextualizedFeaturizer[X, Y, C]) extends ContextualizedFeatureExtractor[X, Y, C] {
    def applyOnGroup[A1 <: X](ga: FeatureGroup[A1])(implicit c: C) = Iterable(ga flatMap featurizer.attachContext(c))
    def apply(a: X)(implicit c: C) = Iterable(featurizer(a)(c))
  }

  case class Concatenated[-X, Y, C](featurizers: Iterable[ContextualizedFeatureExtractor[X, Y, C]]) extends ContextualizedFeatureExtractor[X, Y, C] {
    def apply(a: X)(implicit c: C) = for {
      f ← featurizers
      fga ← f(a)
    } yield fga

    def applyOnGroup[X1 <: X](ga: FeatureGroup[X1])(implicit c: C) = for {
      f ← featurizers
      fga ← f.applyOnGroup(ga)
    } yield fga

    override def ++[X1 <: X](f: ContextualizedFeatureExtractor[X1, Y, C]) = f match {
      case Concatenated(g) => Concatenated(featurizers ++ g)
      case _ => Concatenated(featurizers ++ Iterable(f))
    }
  }

  case class Composed[-X, Y, Z, C, C1 >: C](f1: ContextualizedFeatureExtractor[X, Y, C], f2: ContextualizedFeatureExtractor[Y, Z, C1]) extends ContextualizedFeatureExtractor[X, Z, C] {

    def applyOnGroup[X1 <: X](ga: FeatureGroup[X1])(implicit c: C): Iterable[FeatureGroup[Z]] = {
      val f1a = f1.applyOnGroup(ga)
      for {
        fg1 ← f1a
        fg2 ← f2.applyOnGroup(fg1)
      } yield fg2
    }

    def apply(a: X)(implicit c: C) = {
      val f1a = f1(a)
      for {
        fg1 ← f1a
        fg2 ← f2.applyOnGroup(fg1)
      } yield fg2
    }
  }

}