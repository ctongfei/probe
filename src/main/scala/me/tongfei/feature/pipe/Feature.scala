package me.tongfei.feature.pipe

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait Feature[+A] {

  def group: String
  def value: A

  override def toString = s"$group~$value"
}

object Feature {
  def apply[A](g: String, v: A): Feature[A] = new Feature[A] {
    def group = g
    def value = v
  }

  def unapply[A](f: Feature[A]): Option[(String, A)] = Some((f.group, f.value))
}
