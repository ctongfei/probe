package me.tongfei.probe

/**
 * Represents a featurizer that depends on an implicit context.
 * @author Tongfei Chen
 * @since 0.6.0
 * @tparam C Type of context
 */
trait ContextualizedFeaturizer[-X, +Y, -C] {
  self =>

  import ContextualizedFeaturizer._

  def name: String

  def extract(x: X, c: C): FeatureGroup[Y]

  def attachContext(implicit c: C) = Featurizer.create(name) { x: X => extract(x, c) }

  def appendName(n: String) = create(s"$name-$n") { (x: X, c: C) =>
    extract(x, c).appendName(n)
  }

  def changeName(n: String) = create(n) { (x: X, c: C) =>
    extract(x, c).changeName(n)
  }

  def map[Z](f: Y => Z) = create(name) { (x: X, c: C) =>
    extract(x, c).map(f)
  }

  def contramap[Z](f: Z => X) = create(name) { (z: Z, c: C) =>
    extract(f(z), c)
  }

  def andThen[Z, C1 <: C](f: ContextualizedFeaturizer[Y, Z, C1]) = create(name + "-" + f.name) { (x: X, c: C1) =>
    extract(x, c).flatMap(f.attachContext(c))
  }

  def filter(f: Y => Boolean) = create(name) { (x: X, c: C) =>
    extract(x, c).filter(f)
  }

  def topK(k: Int) = create(name) { (x: X, c: C) =>
    extract(x, c).topK(k)
  }

  def assignWeights(f: Y => Double) = create(name) { (x: X, c: C) =>
    extract(x, c).assignValues(f)
  }

  def uniformWeight = create(name) { (x: X, c: C) =>
    extract(x, c).uniformValue
  }

  def l2Normalize = create(name) { (x: X, c: C) =>
    extract(x, c).l2Normalize
  }

  def l1Normalize = create(name) { (x: X, c: C) =>
    extract(x, c).l1Normalize
  }

  def binarize(threshold: Double) = create(name) { (x: X, c: C) =>
    extract(x, c).binarize(threshold)
  }

  def >>>[Z, C1 <: C](f: ContextualizedFeaturizer[Y, Z, C1]) = andThen(f)

}

object ContextualizedFeaturizer {

  private[probe] def create[X, Y, C](n: String)(f: (X, C) => FeatureGroup[Y]): ContextualizedFeaturizer[X, Y, C] =
    new ContextualizedFeaturizer[X, Y, C] {
      def name = n
      def extract(a: X, ctx: C) = f(a, ctx)
    }

  def count[X, Y, C](name: String)(f: (X, C) => Iterable[Y]) =
    create(name)((x: X, c: C) => FeatureGroup.count(name)(f(x, c)))

  def binary[X, Y, C](name: String)(f: (X, C) => Iterable[Y]) =
    create(name)((x: X, c: C) => BinaryFeatureGroup(name)(f(x, c)))

  def realValued[X, Y, C](name: String)(f: (X, C) => Iterable[(Y, Double)]) =
    create(name)((x: X, c: C) => FeatureGroup(name)(f(x, c)))

  def singleCategorical[X, Y, C](name: String)(f: (X, C) => Y) =
    create(name)((x: X, c: C) => SingleCategoricalFeature(name)(f(x, c)))

  def singleNumerical[X, C](name: String)(f: (X, C) => Double) =
    create(name)((x: X, c: C) => SingleNumericalFeature(name)(f(x, c)))

  def realVector[X, C](name: String)(f: (X, C) => Array[Double]) =
    create(name)((x: X, c: C) => DenseVectorFeature(name)(f(x, c)))

}
