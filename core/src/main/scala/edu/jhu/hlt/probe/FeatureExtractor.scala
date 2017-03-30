package edu.jhu.hlt.probe

/**
 * Represents a feature extractor.
 *
 * @author Tongfei Chen
 * @since 0.5.0
 */
trait FeatureExtractor[-X, +Y] { self =>

  import FeatureExtractor._

  def extractOnGroup[X1 <: X](gx: FeatureGroup[X1]): Iterable[FeatureGroup[Y]]

  def extract(x: X): Iterable[FeatureGroup[Y]]

  def >>>[Z](that: Y => Z) = self map that

  def >>>[Z](that: FeatureExtractor[Y, Z]): FeatureExtractor[X, Z]
    = new Composed(self, that)

  def ++[X1 <: X, Z >: Y](that: FeatureExtractor[X1, Z]): FeatureExtractor[X1, Z]
     = new Concatenated(Iterable(self, that))

  def contramap[W](f: W => X): FeatureExtractor[W, Y] = self match {
    case Trivial(g)       => Trivial(g contramap f)
    case Concatenated(gs) => Concatenated(gs map { _ contramap f })
    case Composed(g, h)   => Composed(g contramap f, h)
  }

  def map[Z](f: Y => Z): FeatureExtractor[X, Z] = self match {
    case Trivial(g)       => Trivial(g map f)
    case Concatenated(gs) => Concatenated(gs map { _ map f})
    case Composed(g, h)   => Composed(g, h map f)
  }

}


object FeatureExtractor {

  case class Trivial[-X, +Y](featurizer: Featurizer[X, Y]) extends FeatureExtractor[X, Y] {
    def extractOnGroup[X1 <: X](gx: FeatureGroup[X1]) = Iterable(gx flatMap featurizer)
    def extract(x: X) = Iterable(featurizer(x))
  }

  case class Concatenated[-X, +Y](featurizers: Iterable[FeatureExtractor[X, Y]]) extends FeatureExtractor[X, Y] {
    def extractOnGroup[X1 <: X](gx: FeatureGroup[X1]) = for {
      f ← featurizers
      fgx ← f.extractOnGroup(gx)
    } yield fgx
    def extract(x: X) = for {
      f ← featurizers
      fgx ← f.extract(x)
    } yield fgx
    override def ++[X1 <: X, Z >: Y](f: FeatureExtractor[X1, Z]) = f match {
      case Concatenated(g) => Concatenated(featurizers ++ g)
      case _ => Concatenated(featurizers ++ Iterable(f))
    }
  }

  case class Composed[-X, Y, Z](f1: FeatureExtractor[X, Y], f2: FeatureExtractor[Y, Z]) extends FeatureExtractor[X, Z] {
    def extractOnGroup[X1 <: X](gx: FeatureGroup[X1]) = {
      val f1x = f1.extractOnGroup(gx)
      for {
        fg1 ← f1x
        fg2 ← f2.extractOnGroup(fg1)
      } yield fg2
    }
    def extract(x: X) = {
      val f1x = f1.extract(x)
      for {
        fg1 ← f1x
        fg2 ← f2.extractOnGroup(fg1)
      } yield fg2
    }
  }

}