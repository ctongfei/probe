package me.tongfei.probe

/**
 * Represents a featurizer that depends on an implicit context.
 * @author Tongfei Chen
 * @since 0.6.0
 * @tparam C Type of context
 */
trait ContextualizedFeaturizer[-X, Y, C] { self =>

  import ContextualizedFeaturizer._

  def name: String

  def extract(x: X, c: C): FeatureGroup[Y]

  def apply(x: X)(implicit c: C) = extract(x, c)

  def attachContext(implicit c: C) = Featurizer(name) { x: X => extract(x, c) }

  def appendName(n: String) = create(s"$name-$n") { (x: X, c: C) =>
    extract(x, c).appendName(n)
  }

  def changeName(n: String) = create(n) { (x: X, c: C) =>
    extract(x, c).changeName(n)
  }

  def map[Z](f: Y => Z) = create(name) { (x: X, c: C) =>
    extract(x, c).map(f)
  }

  def andThen[Z, C1 >: C](f: ContextualizedFeaturizer[Y, Z, C1]) = create(name + "-" + f.name) { (x: X, c: C) =>
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

  def >>>[Z, C1 >: C](f: ContextualizedFeaturizer[Y, Z, C1]) = andThen(f)

}

object ContextualizedFeaturizer {

  private[probe] def create[X, Y, C](n: String)(f: (X, C) => FeatureGroup[Y]): ContextualizedFeaturizer[X, Y, C] =
    new ContextualizedFeaturizer[X, Y, C] {
      def name = n
      def extract(a: X, ctx: C) = f(a, ctx)
    }

}