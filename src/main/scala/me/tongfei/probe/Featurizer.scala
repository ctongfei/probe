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

  def ++[A1 <: A](that: Featurizer[A1, B]): FeaturizerSet[A1, B] = SimpleFeaturizerSet(Iterable(self, that))

  def >>>[C](that: Featurizer[B, C]) = self andThen that

  def >>>[C](that: FeaturizerSet[B, C]) = SimpleFeaturizerSet(Iterable(self)) >>> that

  def cartesianProduct[A1, B1](that: Featurizer[A1, B1]): Featurizer[(A, A1), (B, B1)] = Featurizer(name + "," + that.name) { case (a, a1) =>
    self(a) cartesianProduct that(a1)
  }

  def ×[A1, B1](that: Featurizer[A1, B1]) = cartesianProduct(that)

}

object Featurizer {
  def apply[A, B](n: String)(f: A => FeatureGroup[B]): Featurizer[A, B] = new Featurizer[A, B] {
    def name = n
    def apply(a: A) = f(a)
  }
}