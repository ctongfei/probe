package me.tongfei.probe

/**
  * Represents a feature group with one single feature.
  * @author Tongfei Chen (ctongfei@gmail.com).
  * @since 0.4.3
  */
class SingleNumericalFeature(val name: String)(val value: Double = 1.0) extends FeatureGroup[Unit] {

  def pairs = Iterable(() → value)

}


object SingleNumericalFeature {

  def apply(g: String)(v: Double = 1.0) = new SingleNumericalFeature(g)(v)

}
