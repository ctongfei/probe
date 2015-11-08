package me.tongfei.probe

/**
  * Represents a feature group with one single feature.
  * @author Tongfei Chen (ctongfei@gmail.com).
  * @since 0.4.3
  */
class SingleFeature(val name: String)(val value: Double = 1.0) extends FeatureGroup[Unit] {

  def pairs = Iterable(() → value)

}


object SingleFeature {

  def apply(g: String)(v: Double = 1.0) = new SingleFeature(g)(v)

}
