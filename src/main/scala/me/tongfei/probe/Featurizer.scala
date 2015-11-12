package me.tongfei.probe

/**
  * Represents a feature extractor that extracts a sequence of features with the same group name.
  * @author Tongfei Chen (ctongfei@gmail.com).
  */
trait Featurizer[-A, B] extends (A => FeatureGroup[B]) { self =>

  def name: String

  def map[C](f: B => C): Featurizer[A, C] = Featurizer(name) { a =>
    self(a).map(f)
  }

  def andThen[C](f: Featurizer[B, C]): Featurizer[A, C] = Featurizer(name + "-" + f.name) { a =>
    self(a).flatMap(f)
  }

  def filter(f: B => Boolean): Featurizer[A, B] = Featurizer(name) { a =>
    self(a).filter(f)
  }

  def topK(k: Int): Featurizer[A, B] = Featurizer(name) { a =>
    self(a).topK(k)
  }

  def assignWeights(f: B => Double): Featurizer[A, B] = Featurizer(name) { a =>
    self(a).assignValues(f)
  }

  def uniformWeight: Featurizer[A, B] = Featurizer(name) { a =>
    self(a).uniformValue
  }

  def binarize(threshold: Double): Featurizer[A, B] = Featurizer(name) { a =>
    self(a).binarize(threshold)
  }

  def >>>[C](that: Featurizer[B, C]) = self andThen that

  def >>>[C](that: FeatureExtractor[B, C]) = featurizerToFeatureExtractor(self) >>> that

  def cartesianProduct[A1, B1](that: Featurizer[A1, B1]): Featurizer[(A, A1), (B, B1)] = Featurizer(name + "," + that.name) { case (a, a1) =>
    self(a) cartesianProduct that(a1)
  }

  def ×[A1, B1](that: Featurizer[A1, B1]) = cartesianProduct(that)

}

object Featurizer {

  private[probe] def apply[A, B](n: String)(f: A => FeatureGroup[B]): Featurizer[A, B] = new Featurizer[A, B] {
    def name = n
    def apply(a: A) = f(a)
  }

  def count[A, B](name: String)(f: A => Iterable[B]) = apply(name)((a: A) => FeatureGroup.count(name)(f(a)))

  def binary[A, B](name: String)(f: A => Iterable[B]) = apply(name)((a: A) => BinaryFeatureGroup(name)(f(a)))

  def realValued[A, B](name: String)(f: A => Iterable[(B, Double)]) = apply(name)((a: A) => FeatureGroup(name)(f(a)))

  def singleCategorical[A, B](name: String)(f: A => B) = apply(name)((a: A) => SingleCategoricalFeature(name)(f(a)))

  def singleNumerical[A](name: String)(f: A => Double) = apply(name)((a: A) => SingleNumericalFeature(name)(f(a)))

  def realVector[A](name: String)(f: A => Array[Double]) = apply(name)((a: A) => DenseVectorFeature(name)(f(a)))
}