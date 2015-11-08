package me.tongfei.probe

/**
  * Represents a feature group that is formed by packing a dense feature into a feature vector.
  * @author Tongfei Chen (ctongfei@gmail.com).
  * @since 0.4.4
  */
class DenseVectorFeature(val name: String)(val vector: Array[Double]) extends FeatureGroup[Int] {

  def pairs = vector.zipWithIndex.map(_.swap)

}

object DenseVectorFeature {

  def apply(g: String)(v: Array[Double]) = new DenseVectorFeature(g)(v)

}
