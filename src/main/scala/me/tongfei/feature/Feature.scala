package me.tongfei.feature

/**
 * Represents a basic feature.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait Feature { self =>

  def key: String
  def value: String
  def name: String = key + "~" + value
  def weight: Double

  override def equals(that: Any) = that match {
    case that: Feature => this.name == that.name
    case _ => false
  }

  override def toString = key + "~" + value + ": " + weight.toString

  override def hashCode = toString.hashCode

}

case class BinaryFeature(key: String, value: String) extends Feature {
  final val weight = 1.0
  override final val name = key + "~" + value
}

case class RealValuedFeature(key: String, value: String, weight: Double) extends Feature {
  override final val name = key + "~" + value
}

case class ProductFeature(f1: Feature, f2: Feature) extends Feature {
  val key = f1.key + "," + f2.key
  val value = f1.value + "," + f2.value
  val weight = f1.weight * f2.weight
}

case class JoinedFeature(f1: Feature, f2: Feature) extends Feature {
  require(f1.value == f2.value)
  val key = f1.key + "=" + f2.key
  val value = f1.value
  val weight = f1.weight * f2.weight
}

case class EqualityFeature(f1: Feature, f2: Feature) extends Feature {
  require(f1.value == f2.value)
  val key = f1.key + "=" + f2.key
  val value = ""
  val weight = f1.weight * f2.weight
}


object Feature {

  def apply(g: String) = BinaryFeature(g, "")
  def apply(g: String, v: String) = BinaryFeature(g, v)
  def apply(g: String, w: Double) = RealValuedFeature(g, "", w)
  def apply(g: String, v: String, w: Double) = RealValuedFeature(g, v, w)

}
