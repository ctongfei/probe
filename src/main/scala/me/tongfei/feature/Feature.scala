package me.tongfei.feature

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait Feature[+A] {

  def group: String
  def value: A

  def name = toString
  override def toString = s"$group~$value"
}

object Feature {
  def apply[A](g: String, v: A): Feature[A] = new Feature[A] {
    def group = g
    def value = v
  }

  def unapply[A](f: Feature[A]): Option[(String, A)] = Some((f.group, f.value))
}

case class ProductFeature[+A, +B](ga: String, va: A, gb: String, vb: B) extends Feature[(A, B)] {
  def group: String = ga + "," + gb
  def value: (A, B) = (va, vb)
}

case class EqualityFeature(ga: String, gb: String) extends Feature[Unit] {
  def group: String = ga + "=" + gb
  def value: Unit = ()
}
