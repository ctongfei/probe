package me.tongfei.probe

import scala.collection._

/**
 * Represents a feature extractor that yields multiple feature groups within one pass of feature extraction.
 * @example
 *   An example would be named entities with their types.
 *   <p> '''f''',,NE,,(''Hillary Clinton is elected as the Democratic Party's presidential candidate'') = { {{{
 *     ne.PER~Hillary Clinton -> 1.0
 *     ne.ORG~Democratic Party -> 1.0
 *   }}}
 *   } </p>
 * @author Tongfei Chen
 * @since 0.7.0
 */
trait FeaturizerFamily[-A, +B, TB] extends FeatureExtractor[A, B] { self =>

  def name(tag: TB): String
  def extractWithTags(x: A): Iterable[(TB, B, Double)]

  def extractOnGroup[A1 <: A](gx: FeatureGroup[A1]): Iterable[FeatureGroup[B]] = {
    val m = mutable.HashMap[TB, mutable.ArrayBuffer[(B, Double)]]()
    for ((x, wx) <- gx.pairs) {
      for ((ty, y, wy) <- extractWithTags(x)) {
        if (!m.contains(ty)) m += ty -> mutable.ArrayBuffer[(B, Double)]()
        m(ty) += y -> (wx * wy)
      }
    }
    m.map { case (tag, list) => FeatureGroup(name(tag))(list) }
  }

  def extract(x: A): Iterable[FeatureGroup[B]] = {
    val m = mutable.HashMap[TB, mutable.ArrayBuffer[(B, Double)]]()
    for ((tag, y, w) <- extractWithTags(x)) {
      if (!m.contains(tag)) m += tag -> mutable.ArrayBuffer[(B, Double)]()
      m(tag) += y -> w
    }
    m.map { case (tag, list) => FeatureGroup(name(tag))(list) }
  }

  override def contramap[W](f: W => A): FeaturizerFamily[W, B, TB] = new FeaturizerFamily[W, B, TB] {
    def name(tag: TB) = self.name(tag)
    def extractWithTags(w: W) = self.extractWithTags(f(w))
  }

  override def map[C](f: B => C): FeaturizerFamily[A, C, TB] = new FeaturizerFamily[A, C, TB] {
    def name(tag: TB) = self.name(tag)
    def extractWithTags(a: A) = self.extractWithTags(a).map { case (t, b, w) => (t, f(b), w) }
  }

  def <*>[C, D](that: Featurizer[C, D]): FeaturizerFamily[(A, C), (B, D), TB] = new FamilyProductSingle(self, that)

  def <*>[C, D, TD](that: FeaturizerFamily[C, D, TD]): FeaturizerFamily[(A, C), (B, D), (TB, TD)] = new FamilyProductFamily(self, that)

  override def toString = {
    try {
      name("?".asInstanceOf[TB])
    }
    catch {
      case ex: Exception => super.toString
    }
  }

}

object FeaturizerFamily {

  def count[A, B, TB](n: String)(f: A => Iterable[(TB, B)]): FeaturizerFamily[A, B, TB] =
    new FeaturizerFamily[A, B, TB] {
      def name(tag: TB) = s"$n.$tag"
      def extractWithTags(x: A) = f(x) map { case (t, b) => (t, b, 1.0) }
    }

  def binary[A, B, TB](n: String)(f: A => Iterable[(TB, B)]): FeaturizerFamily[A, B, TB] =
    new FeaturizerFamily[A, B, TB] {
      def name(tag: TB) = s"$n.$tag"
      def extractWithTags(x: A) = f(x).toSet map { tb: (TB, B) => (tb._1, tb._2, 1.0) }
    }

  def realValued[A, B, TB](n: String)(f: A => Iterable[(TB, B, Double)]): FeaturizerFamily[A, B, TB] =
    new FeaturizerFamily[A, B, TB] {
      def name(tag: TB) = s"$n.$tag"
      def extractWithTags(x: A) = f(x)
    }

}
