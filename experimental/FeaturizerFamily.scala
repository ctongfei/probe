package me.tongfei.probe

import scala.collection._
/**
 * @author Tongfei Chen
 * @since 0.7.0
 */
trait FeaturizerFamily[-X, +Y] extends FeatureExtractor[X, Y] { self =>

  def name: String
  def extractWithTags(x: X): Iterable[(String, Y, Double)]

  def extractOnGroup[X1 <: X](gx: FeatureGroup[X1]): Iterable[FeatureGroup[Y]] = {
    val m = mutable.HashMap[String, mutable.ArrayBuffer[(Y, Double)]]()
    for ((x, wx) <- gx.pairs) {
      for ((tag, y, w) <- extractWithTags(x)) {
        if (!m.contains(tag)) m += tag -> mutable.ArrayBuffer[(Y, Double)]()
        m(tag) += y -> (wx * w)
      }
    }
    m.map { case (tag, list) => FeatureGroup(s"$name-$tag")(list) }
  }

  def extract(x: X): Iterable[FeatureGroup[Y]] = {
    val m = mutable.HashMap[String, mutable.ArrayBuffer[(Y, Double)]]()
    for ((tag, y, w) <- extractWithTags(x)) {
      if (!m.contains(tag)) m += tag -> mutable.ArrayBuffer[(Y, Double)]()
      m(tag) += y -> w
    }
    m.map { case (tag, list) => FeatureGroup(s"$name-$tag")(list) }
  }

  override def contramap[W](f: W => X) = new FeaturizerFamily[W, Y] {
    def name = self.name
    def extractWithTags(w: W) = self.extractWithTags(f(w))
  }
}

object FeaturizerFamily {

  def count[A, B](n: String)(f: A => Iterable[(String, B)]) = new FeaturizerFamily[A, B] {
    def name = n
    def extractWithTags(x: A) = f(x) map { case (t, y) => (t, y, 1.0) }
  }

  def binary[A, B](n: String)(f: A => Iterable[(String, B)]) = new FeaturizerFamily[A, B] {
    def name = n
    def extractWithTags(x: A) = f(x).toSet.map { case (t, y) => (t, y, 1.0) }
  }

  def realValued[A, B](n: String)(f: A => Iterable[(String, B, Double)]) = new FeaturizerFamily[A, B] {
    def name = n
    def extractWithTags(x: A) = f(x)
  }

}