package me.tongfei.feature

/**
 * Represents a basic feature.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait Feature { self =>

  def group: String
  def value: String
  def weight: Double = 1.0

  def $(w: Double) = Feature(self.group, self.value, w)

  override def toString = s"$group~$value $$ $weight"

}

object Feature {

  def apply(g: String, v: Any) = new Feature {
    def group = g
    def value = v.toString
  }

  def apply(g: String, v: Any, w: Double) = new Feature {
    def group = g
    def value = v.toString
    override def weight = w
  }

}