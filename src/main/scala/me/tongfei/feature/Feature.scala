package me.tongfei.feature

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait Feature[+A] {

  def name: String
  def key: A

  override def toString = s"$name~$key"

  override def equals(that: Any) = that match {
    case that: Feature[A] => this.name == that.name && this.key == that.key
    case _ => false
  }

  override def hashCode = toString.hashCode
}

object Feature {
  def apply[A](g: String, v: A): Feature[A] = new Feature[A] {
    def name = g
    def key = v
  }

  def unapply[A](f: Feature[A]): Option[(String, A)] = Some((f.name, f.key))
}



case class SimpleFeature[+A](name: String, key: A) extends Feature[A]

case class ProductFeature[+A, +B](f1: Feature[A], f2: Feature[B]) extends Feature[(A, B)] {
  def name = f1.name + "," + f2.name
  def key = (f1.key, f2.key)
}

case class EqualityFeature(name1: String, name2: String) extends Feature[Unit] {
  def name: String = name1 + "=" + name2
  def key: Unit = ()
}
