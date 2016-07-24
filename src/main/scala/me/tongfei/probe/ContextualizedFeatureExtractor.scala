package me.tongfei.probe

/**
  * Represents a contextualized feature extractor, which contains a list of contextualized featurizers.
  * @author Tongfei Chen (ctongfei@gmail.com).
  * @since 0.6.0
  */
trait ContextualizedFeatureExtractor[-X, +Y, -C] { self =>

  import ContextualizedFeatureExtractor._
  /**
    * Applies this feature extractor on every key in a feature group.
    */
  def extractOnGroup[X1 <: X](ga: FeatureGroup[X1], c: C): Iterable[FeatureGroup[Y]]

  /**
    * Applies this feature vector on an element.
    * @return A list of feature groups extracted out of this input object.
    */
  def extract(a: X, c: C): Iterable[FeatureGroup[Y]]

  /**
    * Composes two feature extractors by first piping an object through the first extractor and then the second.
    * @return A composed feature extractor.
    */
  def >>>[Z, C1 <: C](that: ContextualizedFeatureExtractor[Y, Z, C1]): ContextualizedFeatureExtractor[X, Z, C1]
    = new Composed(self, that)

  /**
    * Concatenates two feature extractors.
    * @return A concatenated feature extractor.
    */
  def ++[X1 <: X, C1 <: C, Z >: Y](that: ContextualizedFeatureExtractor[X1, Z, C1]): ContextualizedFeatureExtractor[X1, Z, C1]
    = new Concatenated(Iterable(self, that))

  def contramap[W](f: W => X): ContextualizedFeatureExtractor[W, Y, C] = self match {
    case Trivial(g)       => Trivial(g contramap f)
    case Concatenated(gs) => Concatenated(gs map { _ contramap f })
    case Composed(g, h)   => Composed(g contramap f, h)
  }

  def map[Z](f: Y => Z): ContextualizedFeatureExtractor[X, Z, C] = self match {
    case Trivial(g)       => Trivial(g map f)
    case Concatenated(gs) => Concatenated(gs map { _ map f })
    case Composed(g, h)   => Composed(g, h map f)
  }

}

object ContextualizedFeatureExtractor {

  case class Trivial[-X, Y, -C](featurizer: ContextualizedFeaturizer[X, Y, C]) extends ContextualizedFeatureExtractor[X, Y, C] {
    def extractOnGroup[A1 <: X](ga: FeatureGroup[A1], c: C) = Iterable(ga flatMap featurizer.attachContext(c))
    def extract(a: X, c: C) = Iterable(featurizer(a)(c))
  }

  case class Concatenated[-X, Y, -C](featurizers: Iterable[ContextualizedFeatureExtractor[X, Y, C]]) extends ContextualizedFeatureExtractor[X, Y, C] {
    def extract(a: X, c: C) = for {
      f ← featurizers
      fga ← f.extract(a, c)
    } yield fga

    def extractOnGroup[X1 <: X](ga: FeatureGroup[X1], c: C) = for {
      f ← featurizers
      fga ← f.extractOnGroup(ga, c)
    } yield fga

    override def ++[X1 <: X, C1 <: C, Z >: Y](f: ContextualizedFeatureExtractor[X1, Z, C1]) = f match {
      case Concatenated(g) => Concatenated(featurizers ++ g)
      case _ => Concatenated(featurizers ++ Iterable(f))
    }
  }

  case class Composed[-X, Y, Z, -C](f1: ContextualizedFeatureExtractor[X, Y, C], f2: ContextualizedFeatureExtractor[Y, Z, C]) extends ContextualizedFeatureExtractor[X, Z, C] {

    def extractOnGroup[X1 <: X](ga: FeatureGroup[X1], c: C): Iterable[FeatureGroup[Z]] = {
      val f1a = f1.extractOnGroup(ga, c)
      for {
        fg1 ← f1a
        fg2 ← f2.extractOnGroup(fg1, c)
      } yield fg2
    }

    def extract(a: X, c: C) = {
      val f1a = f1.extract(a, c)
      for {
        fg1 ← f1a
        fg2 ← f2.extractOnGroup(fg1, c)
      } yield fg2
    }
  }

}