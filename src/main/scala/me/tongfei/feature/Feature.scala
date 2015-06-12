package me.tongfei.feature

/**
 * Represents a basic feature.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait Feature { self =>

  def key: String
  def value: String
  def name: String = s"$key~$value"
  def weight: Double

  override def toString = s"$name: $weight"

}

case class BinaryFeature(key: String, value: String) extends Feature {
  def weight = 1.0
}

case class RealValuedFeature(key: String, value: String, weight: Double) extends Feature

case class ProductFeature(f1: Feature, f2: Feature) extends Feature {
  def key = f1.key + "," + f2.key
  def value = f1.value + "," + f2.value
  def weight = f1.weight * f2.weight
}

case class JoinedFeature(f1: Feature, f2: Feature) extends Feature {
  require(f1.value == f2.value)
  def key = f1.key + "=" + f2.key
  def value = ""
  def weight = f1.weight * f2.weight
}

object Feature {

  def apply(g: String) = BinaryFeature(g, "")

  def apply(g: String, v: String) = BinaryFeature(g, v)

  def apply(g: String, w: Double) = RealValuedFeature(g, "", w)

  def apply(g: String, v: String, w: Double) = RealValuedFeature(g, v, w)

}
