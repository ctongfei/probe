package me.tongfei.probe

import scala.collection._
/**
 * @author Tongfei Chen
 * @since 0.7.0
 */
trait FeaturizerFamily[-A, +B] extends FeatureExtractor[A, B] { self =>

  def name: String
  def extractWithTags(x: A): Iterable[(String, B, Double)]

  def extractOnGroup[A1 <: A](gx: FeatureGroup[A1]): Iterable[FeatureGroup[B]] = {
    val m = mutable.HashMap[String, mutable.ArrayBuffer[(B, Double)]]()
    for ((x, wx) <- gx.pairs) {
      for ((ty, y, wy) <- extractWithTags(x)) {
        if (!m.contains(ty)) m += ty -> mutable.ArrayBuffer[(B, Double)]()
        m(ty) += y -> (wx * wy)
      }
    }
    m.map { case (tag, list) => FeatureGroup(s"$name-$tag")(list) }
  }

  def extract(x: A): Iterable[FeatureGroup[B]] = {
    val m = mutable.HashMap[String, mutable.ArrayBuffer[(B, Double)]]()
    for ((tag, y, w) <- extractWithTags(x)) {
      if (!m.contains(tag)) m += tag -> mutable.ArrayBuffer[(B, Double)]()
      m(tag) += y -> w
    }
    m.map { case (tag, list) => FeatureGroup(s"$name-$tag")(list) }
  }

  override def contramap[W](f: W => A) = new FeaturizerFamily[W, B] {
    def name = self.name
    def extractWithTags(w: W) = self.extractWithTags(f(w))
  }

  def <*>[C, D](that: Featurizer[C, D]): FeaturizerFamily[(A, C), (B, D)] = new FeaturizerFamily[(A, C), (B, D)] {
    def name = s"${self.name}-${that.name}"
    def extractWithTags(ac: (A, C)) = {
      val (a, c) = ac
      for {
        (tb, b, wb) <- self.extractWithTags(a)
        (d, wd) <- that.extract(c).pairs
      } yield (tb, (b, d), wb * wd)
    }
  }

  def <*>[C, D](that: FeaturizerFamily[C, D]): FeaturizerFamily[(A, C), (B, D)] = new FeaturizerFamily[(A, C), (B, D)] {
    def name=s"${self.name}-${that.name}"
    def extractWithTags(ac: (A, C)) = {
      val (a, c) = ac
      for {
        (tb, b, wb) <- self.extractWithTags(a)
        (td, d, wd) <- that.extractWithTags(c)
      } yield (s"$tb-$td", (b, d), wb * wd)
    }
  }

}

object FeaturizerFamily {

  def count[A, B](n: String)(f: A => Iterable[(String, B)]) = new FeaturizerFamily[A, B] {
    def name = n
    def extractWithTags(x: A) = f(x) map { case (t, b) => (t, b, 1.0) }
  }

  def binary[A, B](n: String)(f: A => Iterable[(String, B)]) = new FeaturizerFamily[A, B] {
    def name = n
    def extractWithTags(x: A) = f(x).toSet.map { tb: (String, B) => (tb._1, tb._2, 1.0) }
  }

  def realValued[A, B](n: String)(f: A => Iterable[(String, B, Double)]) = new FeaturizerFamily[A, B] {
    def name = n
    def extractWithTags(x: A) = f(x)
  }

}