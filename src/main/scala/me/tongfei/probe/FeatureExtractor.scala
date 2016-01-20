package me.tongfei.probe

/**
 * Represents a feature extractor.
 * @author Tongfei Chen
 * @since 0.5.0
 */
sealed trait FeatureExtractor[-X, Y] { self =>

  import FeatureExtractor._

  def applyOnGroup[X1 <: X](gx: FeatureGroup[X1]): Iterable[FeatureGroup[Y]]

  def apply(x: X): Iterable[FeatureGroup[Y]]

  def >>>[Z](that: FeatureExtractor[Y, Z]): FeatureExtractor[X, Z]
    = new Composed(self, that)

  def ++[X1 <: X](that: FeatureExtractor[X1, Y]): FeatureExtractor[X1, Y]
     = new Concatenated(Iterable(self, that))

}


object FeatureExtractor {

  case class Trivial[-X, Y](featurizer: Featurizer[X, Y]) extends FeatureExtractor[X, Y] {
    def applyOnGroup[X1 <: X](gx: FeatureGroup[X1]) = Iterable(gx flatMap featurizer)
    def apply(x: X) = Iterable(featurizer(x))
  }

  case class Concatenated[-X, Y](featurizers: Iterable[FeatureExtractor[X, Y]]) extends FeatureExtractor[X, Y] {
    def applyOnGroup[X1 <: X](gx: FeatureGroup[X1]) = for {
      f ← featurizers
      fgx ← f.applyOnGroup(gx)
    } yield fgx
    def apply(x: X) = for {
      f ← featurizers
      fgx ← f(x)
    } yield fgx
    override def ++[X1 <: X](f: FeatureExtractor[X1, Y]) = f match {
      case Concatenated(g) => Concatenated(featurizers ++ g)
      case _ => Concatenated(featurizers ++ Iterable(f))
    }
  }

  case class Composed[-X, Y, Z](f1: FeatureExtractor[X, Y], f2: FeatureExtractor[Y, Z]) extends FeatureExtractor[X, Z] {
    def applyOnGroup[X1 <: X](gx: FeatureGroup[X1]) = {
      val f1x = f1.applyOnGroup(gx)
      for {
        fg1 ← f1x
        fg2 ← f2.applyOnGroup(fg1)
      } yield fg2
    }
    def apply(x: X) = {
      val f1x = f1(x)
      for {
        fg1 ← f1x
        fg2 ← f2.applyOnGroup(fg1)
      } yield fg2
    }
  }

}