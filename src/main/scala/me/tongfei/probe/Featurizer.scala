package me.tongfei.probe

import scala.language.implicitConversions

/**
  * Represents a feature extractor that extracts a sequence of features with the same group name.
  * @author Tongfei Chen (ctongfei@gmail.com).
  */
trait Featurizer[-X, Y] { self =>

  def name: String

  def extract(a: X): FeatureGroup[Y]

  def apply(a: X) = extract(a)

  def appendName(n: String) = Featurizer(s"$name-$n") { (a: X) =>
    extract(a).appendName(n)
  }

  def changeName(n: String) = Featurizer(n) { (a: X) =>
    extract(a).changeName(n)
  }

  def map[Z](f: Y => Z) = Featurizer(name) { (a: X) =>
    extract(a).map(f)
  }

  def andThen[Z](f: Featurizer[Y, Z]) = Featurizer(name + "-" + f.name) { (a: X) =>
    extract(a).flatMap(f)
  }

  def filter(f: Y => Boolean) = Featurizer(name) { (a: X) =>
    extract(a).filter(f)
  }

  def topK(k: Int): Featurizer[X, Y] = Featurizer(name) { (a: X) =>
    extract(a).topK(k)
  }

  def assignWeights(f: Y => Double) = Featurizer(name) { (a: X) =>
    extract(a).assignValues(f)
  }

  def uniformWeight = Featurizer(name) { (a: X) =>
    extract(a).uniformValue
  }

  def l2Normalize = Featurizer(name) { (a: X) =>
    extract(a).l2Normalize
  }

  def l1Normalize = Featurizer(name) { (a: X) =>
    extract(a).l1Normalize
  }

  def binarize(threshold: Double) = Featurizer(name) { (a: X) =>
    extract(a).binarize(threshold)
  }

  //TODO: optimization?
  def discretize(thresholds: Seq[Double]) = FeatureExtractor.Concatenated(thresholds map { t =>
    FeatureExtractor.Trivial(self binarize t appendName s"$t+")
  })

  def >>>[Z](that: Featurizer[Y, Z]) = self andThen that

  def >>>[Z](that: FeatureExtractor[Y, Z]) = featurizerToFeatureExtractor(self) >>> that

  def *[X1 <: X, Y1](that: Featurizer[X1, Y1]): Featurizer[X1, (Y, Y1)] = Featurizer(name + "," + that.name) { a =>
    self.extract(a) cartesianProduct that.extract(a)
  }

  def <*>[X1, Y1](that: Featurizer[X1, Y1]): Featurizer[(X, X1), (Y, Y1)] = Featurizer(name + "," + that.name) { case (a, a1) =>
    self.extract(a) cartesianProduct that.extract(a1)
  }

  def ×[X1, Y1](that: Featurizer[X1, Y1]) = <*>(that)

}

object Featurizer {

  private[probe] def apply[A, B](n: String)(f: A => FeatureGroup[B]): Featurizer[A, B] = new Featurizer[A, B] {
    def name = n
    def extract(a: A) = f(a)
  }

  implicit def pure[A, B](featurizer: Featurizer[A, B]): ContextualizedFeaturizer[A, B, Any] =
    ContextualizedFeaturizer.create(featurizer.name) { (x: A, c: Any) => featurizer(x) }

  def count[A, B](name: String)(f: A => Iterable[B]) = apply(name)((a: A) => FeatureGroup.count(name)(f(a)))

  def binary[A, B](name: String)(f: A => Iterable[B]) = apply(name)((a: A) => BinaryFeatureGroup(name)(f(a)))

  def realValued[A, B](name: String)(f: A => Iterable[(B, Double)]) = apply(name)((a: A) => FeatureGroup(name)(f(a)))

  def singleCategorical[A, B](name: String)(f: A => B) = apply(name)((a: A) => SingleCategoricalFeature(name)(f(a)))

  def singleNumerical[A](name: String)(f: A => Double) = apply(name)((a: A) => SingleNumericalFeature(name)(f(a)))

  def realVector[A](name: String)(f: A => Array[Double]) = apply(name)((a: A) => DenseVectorFeature(name)(f(a)))

}