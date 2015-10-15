package me.tongfei.feature

import scala.language.reflectiveCalls

/**
 * Represents a general featurizer that extracts a sequence of features
 * of an object of a specific type.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait Featurizer[-A, +B] extends (A => FeatureList[B]) { self =>

  def map[C](f: B => C): Featurizer[A, C] = new Featurizer[A, C] {
    def apply(a: A) = self(a).map(f)
  }

  def andThen[C](f: Featurizer[B, C]): Featurizer[A, C] = new Featurizer[A, C] {
    def apply(a: A) = self(a).flatMap(f)
  }

  def filter(f: B => Boolean): Featurizer[A, B] = new Featurizer[A, B] {
    def apply(a: A) = self(a).filter(f)
  }

  def topK(k: Int): Featurizer[A, B] = new Featurizer[A, B] {
    def apply(a: A) = self(a).topK(k)
  }

  def assignWeights(f: B => Double): Featurizer[A, B] = new Featurizer[A, B] {
    def apply(a: A) = self(a).assignWeights(f)
  }

  def ++[A1 <: A, B1 >: B](that: Featurizer[A1, B1]): Featurizer[A1, B1] = new Featurizer[A1, B1] {
    def apply(a: A1) = self(a) ++ that(a)
  }

  def *[A1 <: A, C](that: Featurizer[A1, C]): Featurizer[A1, (B, C)] = new Featurizer[A1, (B, C)] {
    def apply(a: A1) = self(a) * that(a)
  }

  def =?=[A1, B1 >: B](that: Featurizer[A1, B1]): Featurizer[(A, A1), Unit] = new Featurizer[(A, A1), Unit] {
    def apply(a: (A, A1)) = self(a._1) =?= that(a._2)
  }

  def uniformWeight: Featurizer[A, B] = new Featurizer[A, B] {
    def apply(a: A) = self(a).uniformWeight
  }

  def binarize(threshold: Double): Featurizer[A, B] = new Featurizer[A, B] {
    def apply(a: A) = self(a).binarize(threshold)
  }

  //region Symbolic aliases
  def >>>[C](f: Featurizer[B, C]) = self andThen f
  //endregion
}

object Featurizer {

//  /** Returns the implicit arrow (the Kleisli category on [[me.tongfei.feature.pipe.FeatureList
//    * FeatureList]]s) on featurizers. */
//  implicit object Arrow extends Arrow[Featurizer] {
//    def lift[X, Y](f: X => Y) = new Featurizer[X, Y] {
//      def apply(x: X) = FeatureList(Iterable(Feature("", f(x)) → 1.0))
//    }
//    def compose[X, Y, Z](g: Featurizer[Y, Z], f: Featurizer[X, Y]) = f andThen g
//
//    def id[X] = new Featurizer[X, X] {
//      def apply(x: X) = FeatureList(Iterable(Feature("", x) → 1.0))
//    }
//
//    def apply1[X, Y, Z](f: Featurizer[X, Y]): Featurizer[(X, Z), (Y, Z)] = new Featurizer[(X, Z), (Y, Z)] {
//      def apply(xz: (X, Z)) = {
//        val (x, z) = xz
//        f(x).map(y => (y, z))
//      }
//    }
//
//    def apply2[X, Y, Z](f: Featurizer[X, Y]): Featurizer[(Z, X), (Z, Y)] = new Featurizer[(Z, X), (Z, Y)] {
//      def apply(zx: (Z, X)) = {
//        val (z, x) = zx
//        f(x).map(y => (z, y))
//      }
//    }
//  }
}
