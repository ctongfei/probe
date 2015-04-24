package me.tongfei.feature

/**
 * Represents a basic feature.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait Feature { self =>

  def key: String
  def value: String
  def weight: Double

  def $(w: Double) = Feature(self.key, self.value, w)

  override def toString = s"$key~$value: $weight"

}

case class BinaryFeature(key: String, value: String) extends Feature {
  def weight = 1.0
}

case class RealValuedFeature(key: String, value: String, weight: Double) extends Feature

object Feature {

  def apply(g: String) = BinaryFeature(g, "")

  def apply(g: String, v: String) = BinaryFeature(g, v)

  def apply(g: String, w: Double) = RealValuedFeature(g, "", w)

  def apply(g: String, v: String, w: Double) = RealValuedFeature(g, v, w)

}
