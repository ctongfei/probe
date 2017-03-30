package edu.jhu.hlt.probe

/**
  * Represents a single categorical feature.
 *
 * @author Tongfei Chen (ctongfei@gmail.com).
  * @since 0.5.0
  */
class SingleCategoricalFeature[A](val name: String)(val key: A) extends BinaryFeatureGroup[A] {
  def keys = Iterable(key)
}


object SingleCategoricalFeature {

  def apply[A](g: String)(k: A) = new SingleCategoricalFeature(g)(k)

}
