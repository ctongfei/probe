package me.tongfei.probe

import scala.language.implicitConversions

/**
  * Represents a feature extractor that extracts a sequence of features with the same group name.
  * @author Tongfei Chen (ctongfei@gmail.com).
  */
trait Featurizer[-X, Y] extends ContextualizedFeaturizer[X, Y, Any] { self =>

  import Featurizer._

  def extract(x: X, c: Any) = extract(x)

  def name: String

  def extract(a: X): FeatureGroup[Y]

  override def appendName(n: String) = create(s"$name-$n") { (a: X) =>
    extract(a).appendName(n)
  }

  override def changeName(n: String) = create(n) { (a: X) =>
    extract(a).changeName(n)
  }

  override def map[Z](f: Y => Z) = create(name) { (a: X) =>
    extract(a).map(f)
  }

  def andThen[Z](f: Featurizer[Y, Z]) = create(name + "-" + f.name) { (a: X) =>
    extract(a).flatMap(f)
  }

  override def filter(f: Y => Boolean) = create(name) { (a: X) =>
    extract(a).filter(f)
  }

  override def topK(k: Int): Featurizer[X, Y] = create(name) { (a: X) =>
    extract(a).topK(k)
  }

  override def assignWeights(f: Y => Double) = create(name) { (a: X) =>
    extract(a).assignValues(f)
  }

  override def uniformWeight = create(name) { (a: X) =>
    extract(a).uniformValue
  }

  override def l2Normalize = create(name) { (a: X) =>
    extract(a).l2Normalize
  }

  override def l1Normalize = create(name) { (a: X) =>
    extract(a).l1Normalize
  }

  override def binarize(threshold: Double) = create(name) { (a: X) =>
    extract(a).binarize(threshold)
  }

  //TODO: optimization?
  def discretize(thresholds: Seq[Double]) = FeatureExtractor.Concatenated(thresholds map { t =>
    FeatureExtractor.Trivial(self binarize t appendName s"$t+")
  })

  def >>>[Z](that: Featurizer[Y, Z]) = self andThen that

  def >>>[Z](that: FeatureExtractor[Y, Z]) = featurizerToFeatureExtractor(self) >>> that

  def *[X1 <: X, Y1](that: Featurizer[X1, Y1]): Featurizer[X1, (Y, Y1)] = create(name + "," + that.name) { a =>
    self.extract(a) cartesianProduct that.extract(a)
  }

  def <*>[X1, Y1](that: Featurizer[X1, Y1]): Featurizer[(X, X1), (Y, Y1)] = create(name + "," + that.name) { case (a, a1) =>
    self.extract(a) cartesianProduct that.extract(a1)
  }

  def ×[X1, Y1](that: Featurizer[X1, Y1]) = <*>(that)

}

object Featurizer {

  private[probe] def create[A, B](n: String)(f: A => FeatureGroup[B]): Featurizer[A, B] = new Featurizer[A, B] {
    def name = n
    def extract(a: A) = f(a)
  }

  def count[A, B](name: String)(f: A => Iterable[B]) = create(name)((a: A) => FeatureGroup.count(name)(f(a)))

  def binary[A, B](name: String)(f: A => Iterable[B]) = create(name)((a: A) => BinaryFeatureGroup(name)(f(a)))

  def realValued[A, B](name: String)(f: A => Iterable[(B, Double)]) = create(name)((a: A) => FeatureGroup(name)(f(a)))

  def singleCategorical[A, B](name: String)(f: A => B) = create(name)((a: A) => SingleCategoricalFeature(name)(f(a)))

  def singleNumerical[A](name: String)(f: A => Double) = create(name)((a: A) => SingleNumericalFeature(name)(f(a)))

  def realVector[A](name: String)(f: A => Array[Double]) = create(name)((a: A) => DenseVectorFeature(name)(f(a)))

}