package edu.jhu.hlt.probe

import scala.language.implicitConversions

/**
  * Represents a feature extractor that extracts a sequence of features with the same group name.
  * @author Tongfei Chen (ctongfei@gmail.com).
  */
trait Featurizer[-A, +B] {
  self =>

  import Featurizer._

  def name: String

  def extract(a: A): FeatureGroup[B]

  def appendName(n: String) = create(s"$name-$n") { (a: A) =>
    extract(a).appendName(n)
  }

  def changeName(n: String) = create(n) { (a: A) =>
    extract(a).changeName(n)
  }

  def map[Z](f: B => Z) = create(name) { (a: A) =>
    extract(a).map(f)
  }

  def contramap[Z](f: Z => A) = create(name) {(a: Z) =>
    extract(f(a))
  }

  def andThen[Z](f: Featurizer[B, Z]) = create(name + "-" + f.name) { (a: A) =>
    extract(a).flatMap(f)
  }

  def filter(f: B => Boolean) = create(name) { (a: A) =>
    extract(a).filter(f)
  }

  def topK(k: Int): Featurizer[A, B] = create(name) { (a: A) =>
    extract(a).topK(k)
  }

  def assignWeights(f: B => Double) = create(name) { (a: A) =>
    extract(a).assignValues(f)
  }

  def uniformWeight = create(name) { (a: A) =>
    extract(a).uniformValue
  }

  def l2Normalize = create(name) { (a: A) =>
    extract(a).l2Normalize
  }

  def l1Normalize = create(name) { (a: A) =>
    extract(a).l1Normalize
  }

  def binarize(threshold: Double) = create(name) { (a: A) =>
    extract(a).binarize(threshold)
  }

  //TODO: optimization?
  def discretize(thresholds: Seq[Double]) = FeatureExtractor.Concatenated(thresholds map { t =>
    FeatureExtractor.Trivial(self binarize t appendName s"$t+")
  })

  def tagWith[A1 <: A, T](t: A1 => T): FeaturizerFamily[A1, B, T] = new FeaturizerFamily[A1, B, T] {
    def name(tag: T) = s"${self.name}.$tag"
    def extractWithTags(x: A1): Iterable[(T, B, Double)] = self.extract(x).pairs.map { case (b, w) => (t(x), b, w) }
  }

  def >>>[Z](that: B => Z) = self map that

  def >>>[Z](that: Featurizer[B, Z]) = self andThen that

  def >>>[Z](that: FeatureExtractor[B, Z]) = featurizerToFeatureExtractor(self) >>> that

  def *[X1 <: A, Y1](that: Featurizer[X1, Y1]): Featurizer[X1, (B, Y1)] = create(name + "," + that.name) { a =>
    self.extract(a) cartesianProduct that.extract(a)
  }

  def <*>[X1, Y1](that: Featurizer[X1, Y1]): Featurizer[(A, X1), (B, Y1)] = new SingleProductSingle(self, that)

  def <*>[C, D, TD](that: FeaturizerFamily[C, D, TD]): FeaturizerFamily[(A, C), (B, D), TD] = new SingleProductFamily(self, that)

  def ×[X1, Y1](that: Featurizer[X1, Y1]) = <*>(that)

}

object Featurizer {

  private[probe] def create[A, B](n: String)(f: A => FeatureGroup[B]): Featurizer[A, B] = new Featurizer[A, B] {
    def name = n
    def extract(a: A) = f(a)
  }

  def count[A, B](name: String)(f: A => Iterable[B]) = create(name)((a: A) => FeatureGroup.count(name)(f(a)))

  def optional[A, B](name: String)(f: A => Option[B]) = create(name)((a: A) => FeatureGroup.optional(name)(f(a)))

  def binary[A, B](name: String)(f: A => Iterable[B]) = create(name)((a: A) => BinaryFeatureGroup(name)(f(a)))

  def realValued[A, B](name: String)(f: A => Iterable[(B, Double)]) = create(name)((a: A) => FeatureGroup(name)(f(a)))

  def singleCategorical[A, B](name: String)(f: A => B) = create(name)((a: A) => SingleCategoricalFeature(name)(f(a)))

  def singleNumerical[A](name: String)(f: A => Double) = create(name)((a: A) => SingleNumericalFeature(name)(f(a)))

  def realVector[A](name: String)(f: A => Array[Double]) = create(name)((a: A) => DenseVectorFeature(name)(f(a)))

}

