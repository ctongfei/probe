package edu.jhu.hlt.probe

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait Feature[+A] {

  def template: String
  def feature: A

  override def toString = s"$template~$feature"

  override def equals(that: Any) = that match {
    case that: Feature[A] => this.template == that.template && this.feature == that.feature
    case _ => false
  }

  override def hashCode = toString.hashCode
}

object Feature {
  def apply[A](g: String, v: A): Feature[A] = new Feature[A] {
    def template = g
    def feature = v
  }

  def unapply[A](f: Feature[A]): Option[(String, A)] = Some((f.template, f.feature))
}

case class SimpleFeature[+A](template: String, feature: A) extends Feature[A]

case class ProductFeature[+A, +B](f1: Feature[A], f2: Feature[B]) extends Feature[(A, B)] {
  def template = s"(${f1.template},${f2.template})"
  def feature = (f1.feature, f2.feature)
}

case class CompositeFeature(func: String, name1: String, name2: String) extends Feature[Unit] {
  def template: String = s"$func($name1,$name2)"
  def feature: Unit = ()
}
