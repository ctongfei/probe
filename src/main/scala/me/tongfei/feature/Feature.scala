package me.tongfei.feature

/**
 * Represents a basic feature. Does not includes the weight.
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.2.0
 */
trait Feature { self =>

  def key: String
  def value: String
  def name: String = key + "~" + value

  override def equals(that: Any) = that match {
    case that: Feature => this.name == that.name
    case _ => false
  }

  override def toString = "[" + name + "]"

  override def hashCode = name.hashCode

}

case class BasicFeature(key: String, value: String) extends Feature {
  override final val name = key + "~" + value
}


case class ProductFeature(f1: Feature, f2: Feature) extends Feature {
  val key = f1.key + "," + f2.key
  val value = f1.value + "," + f2.value
  override final val name = key + "~" + value
}

case class JoinedFeature(f1: Feature, f2: Feature) extends Feature {
  require(f1.value == f2.value)
  val key = f1.key + "=" + f2.key
  val value = f1.value
  override final val name = key + "~" + value
}

case class EqualityFeature(f1: Feature, f2: Feature) extends Feature {
  require(f1.value == f2.value)
  val key = f1.key + "=" + f2.key
  val value = ""
  override final val name = key + "~" + value
}


object Feature {

  def apply(g: String) = BasicFeature(g, "")
  def apply(g: String, v: String) = BasicFeature(g, v)

}
