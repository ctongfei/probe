package me.tongfei.probe

/**
  * Represents a group of features (having the same group name)
  * whose weights are all binary (i.e. all 1 because 0 features shouldn't exist
  * in the iterable sequence of features).
  * @author Tongfei Chen (ctongfei@gmail.com).
  * @since 0.4.0
  */
trait BinaryFeatureGroup[A] extends FeatureGroup[A] { self =>

  def keys: Iterable[A]

  def pairs = keys.map(a => a → 1.0)

  override def features = keys map { a => Feature(name, a) → 1.0 }

  override def assignValues(f: A => Double): FeatureGroup[A] = FeatureGroup.unchecked(name) {
    keys map (k => k → f(k))
  }

}

object BinaryFeatureGroup {

  def apply[A](g: String)(ks: Iterable[A]): BinaryFeatureGroup[A] = new BinaryFeatureGroup[A] {
    def keys = ks.toSet
    def name = g
  }

  private[probe] def fast[A](g: String)(ks: Iterable[A]): BinaryFeatureGroup[A] = new BinaryFeatureGroup[A] {
    def keys = ks
    def name = g
  }

}