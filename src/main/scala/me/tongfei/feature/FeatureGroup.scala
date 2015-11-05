package me.tongfei.feature

/**
  * Represents a group of features (having the same group name)
  * that has real-valued weights.
  * @author Tongfei Chen (ctongfei@gmail.com).
  * @since 0.4.0
  */
trait FeatureGroup[+A] { self =>

  /** The name that designates the group of this feature set. */
  def name: String

  /** The key-value pair of the features. */
  def pairs: Iterable[(A, Double)]

  def features = pairs map { case (k, v) => Feature(name, k) → v }

  def appendName(n: String) = FeatureGroup(name + "-" + n)(pairs)

  def map[B](f: A => B) = FeatureGroup(name) {
    pairs map { case (k, v) => f(k) → v }
  }

  def flatMap[B](f: Featurizer[A, B]) = FeatureGroup(name + "-" + f.name) {
    for {
      (ka, va) ← self.pairs
      (kb, vb) ← f(ka).pairs
    } yield kb → (va * vb)
  }

  def filter(f: A => Boolean) = FeatureGroup(name) {
    pairs filter { case (v, _) => f(v) }
  }

  def compact = FeatureGroup(name) {
    pairs.groupBy(_._1) map { case (f, fs) => f → fs.map(_._2).sum }
  }

  def topK(k: Int) = FeatureGroup(name) {
    compact.pairs.toArray.sortBy(-_._2).take(k)
  }


  /* binary combinators */

  def *[B](that: FeatureGroup[B]): FeatureGroup[(A, B)] = FeatureGroup(name + "," + that.name) {
    for {
      (ka, va) ← self.pairs
      (kb, vb) ← that.pairs
    } yield (ka, kb) → (va * vb)
  }

  def =?=[B >: A](that: FeatureGroup[B]): FeatureGroup[Unit] = FeatureGroup(name + "=" + that.name) {
    for {
      (ka, va) ← self.compact.pairs
      (kb, vb) ← that.compact.pairs if ka == kb
    } yield () → math.min(va, vb)
  }

  /* value manipulation */

  def uniformValue: BinaryFeatureGroup[A] = BinaryFeatureGroup(name) {
    pairs map { _._1 }
  }

  def assignValues(f: A => Double): FeatureGroup[A] = FeatureGroup(name) {
    pairs map { case (k, v) => (k, f(k) * v) }
  }

  def binarize(threshold: Double) = BinaryFeatureGroup(name) {
    pairs filter { _._2 >= threshold } map { _._1 }
  }

  //TODO: binning / discretization
}

object FeatureGroup {

  /**
    * Creates a real-valued feature group.
    * @param g name of feature group
    * @param fvs sequence of (key, value) pairs
    * @tparam A type of key
    */
  def apply[A](g: String)(fvs: Iterable[(A, Double)]): FeatureGroup[A] =
    new FeatureGroup[A] {
      def name = g
      def pairs = fvs
    }

}
