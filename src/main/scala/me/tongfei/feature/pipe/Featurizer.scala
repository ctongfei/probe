package me.tongfei.feature.pipe

import poly.algebra.hkt._
import scala.language.reflectiveCalls

/**
 * Represents a general featurizer that extracts a sequence of features
 * of an object of a specific type.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait Featurizer[-A, +B] extends (A => FeatureList[B]) { self =>

  def apply(a: A): FeatureList[B]

  def map[C](f: B => C): Featurizer[A, C] = new Featurizer[A, C] {
    def apply(a: A) = self(a).map(f)
  }

  def andThen[C](f: Featurizer[B, C]) = new Featurizer[A, C] {
    def apply(a: A) = self(a).flatMap(f)
  }

  def filter(f: B => Boolean) = new Featurizer[A, B] {
    def apply(a: A) = self(a).filter(f)
  }

  def filterWeight(f: Double => Boolean) = new Featurizer[A, B] {
    def apply(a: A) = self(a).filterWeight(f)
  }

  def topK(k: Int): Featurizer[A, B] = new Featurizer[A, B] {
    def apply(a: A) = self(a).topK(k)
  }

  def ++[A1 <: A, B1 >: B](that: Featurizer[A1, B1]) = new Featurizer[A1, B1] {
    def apply(a: A1) = self(a) ++ that(a)
  }

  def *[A1 <: A, C](that: Featurizer[A1, C]) = new Featurizer[A1, (B, C)] {
    def apply(a: A1) = self(a) * that(a)
  }

  def =?=[A1 <: A, B1 >: B](that: Featurizer[A1, B1]) = new Featurizer[A1, Unit] {
    def apply(a: A1) = self(a) =?= that(a)
  }

  def uniformWeight = new Featurizer[A, B] {
    def apply(a: A) = self(a).uniformWeight
  }
}


object Featurizer {

  implicit object Arrow extends Arrow[Featurizer] {
    def lift[X, Y](f: X => Y) = new Featurizer[X, Y] {
      def apply(x: X) = FeatureList(Iterable(Feature("", f(x)) → 1.0))
    }
    def compose[X, Y, Z](g: Featurizer[Y, Z], f: Featurizer[X, Y]) = f andThen g

    def id[X] = new Featurizer[X, X] {
      def apply(x: X) = FeatureList(Iterable(Feature("", x) → 1.0))
    }

    def apply1[X, Y, Z](f: Featurizer[X, Y]): Featurizer[(X, Z), (Y, Z)] = new Featurizer[(X, Z), (Y, Z)] {
      def apply(xz: (X, Z)) = {
        val (x, z) = xz
        f(x).map(y => (y, z))
      }
    }

    def apply2[X, Y, Z](f: Featurizer[X, Y]): Featurizer[(Z, X), (Z, Y)] = new Featurizer[(Z, X), (Z, Y)] {
      def apply(zx: (Z, X)) = {
        val (z, x) = zx
        f(x).map(y => (z, y))
      }
    }
  }
}
