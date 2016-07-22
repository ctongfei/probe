package me.tongfei.probe

import me.tongfei.probe.util._
import scala.annotation.unchecked.{uncheckedVariance => uv}

/**
  * Represents a group of features (having the same group name)
  * that has real-valued weights.
 *
  * @author Tongfei Chen (ctongfei@gmail.com).
  * @since 0.4.0
  */
trait FeatureGroup[+A] { self =>

  /** The name that designates the group/template of this feature set. */
  def name: String

  /** The key-value pair of the features. */
  def pairs: Iterable[(A, Double)]

  def features = pairs map { case (k, v) => Feature(name, k) → v }


  private def compact = FeatureGroup.unchecked(name) {
    pairs.groupBy(_._1) map { case (f, fs) => f → fs.map(_._2).sum }
  }

  // HELPER FUNCTIONS

  def changeName(n: String) = FeatureGroup.unchecked(n)(pairs)

  def appendName(n: String) = FeatureGroup.unchecked(name + "-" + n)(pairs)

  def map[B](f: A => B) = FeatureGroup(name) {
    pairs map { case (k, v) => f(k) → v }
  }

  def mapValues(f: Double => Double) = FeatureGroup.unchecked(name) {
    pairs map { case (k, v) => k → f(v) }
  }

  def flatMap[B](f: Featurizer[A, B]) = FeatureGroup(name + "-" + f.name) {
    for {
      (ka, va) ← self.pairs
      (kb, vb) ← f.extract(ka).pairs
    } yield kb → (va * vb)
  }

  def filter(f: A => Boolean) = FeatureGroup.unchecked(name) {
    pairs filter { case (v, _) => f(v) }
  }

  def topK(k: Int) = FeatureGroup.unchecked(name) {
    pairs.toArray.sortBy(-_._2).take(k)
  }

  def unary_- : FeatureGroup[A] = mapValues(-_)

  def *(k: Double): FeatureGroup[A] = mapValues(_ * k)

  /* binary combinators */

  def +[B >: A](that: FeatureGroup[B]): FeatureGroup[B] = {
    require(name == that.name)
    FeatureGroup(name)(this.pairs ++ that.pairs)
  }

  def -[B >: A](that: FeatureGroup[B]): FeatureGroup[B] = this + (-that)

  def cartesianProduct[B](that: FeatureGroup[B]): FeatureGroup[(A, B)] = FeatureGroup.unchecked(s"($name,${that.name})") {
    for {
      (ka, va) ← self.pairs.view
      (kb, vb) ← that.pairs.view
    } yield (ka, kb) → (va * vb)
  }

  def ×[B](that: FeatureGroup[B]): FeatureGroup[(A, B)] = cartesianProduct(that)

  /* value manipulation */

  def uniformValue: BinaryFeatureGroup[A @uv] = BinaryFeatureGroup.fast(name) {
    pairs map { _._1 }
  }

  def assignValues(f: A => Double): FeatureGroup[A] = FeatureGroup.unchecked(name) {
    pairs map { case (k, v) => (k, f(k) * v) }
  }

  def binarize(threshold: Double): BinaryFeatureGroup[A @uv] = BinaryFeatureGroup.fast(name) {
    pairs filter { _._2 >= threshold } map { _._1 }
  }

  def l1Norm: Double = {
    var sum = 0.0
    for ((k, v) ← pairs) sum += math.abs(v)
    sum
  }

  def l2Norm: Double = {
    var sum = 0.0
    for ((k, v) ← pairs) sum += v * v
    math.sqrt(sum)
  }

  def l2Normalize = {
    val l2 = l2Norm
    this mapValues { _ / l2 }
  }

  def l1Normalize = {
    val l1 = l1Norm
    this mapValues { _ / l1 }
  }

  override def toString = {
    pairs.map { case (k, v) =>
      s"$name~$k:${format(v)}"
    }.mkString(" ")
  }
}

object FeatureGroup {

  def count[A](g: String)(ks: Iterable[A]): FeatureGroup[A] =
    unchecked(g)(ks.map(k => k → 1.0)).compact

  /**
    * Creates a real-valued feature group.
    * @param g name of feature group
    * @param fvs sequence of (key, value) pairs
    * @tparam A type of key
    */
  def apply[A](g: String)(fvs: Iterable[(A, Double)]): FeatureGroup[A] = unchecked(g)(fvs).compact

  private[probe] def unchecked[A](g: String)(fvs: Iterable[(A, Double)]): FeatureGroup[A] =
    new FeatureGroup[A] {
      def name = g
      def pairs = fvs
    }

  def empty[A](g: String): FeatureGroup[A] = new FeatureGroup[A] {
    def pairs = Iterable()
    def name = g
  }

}
