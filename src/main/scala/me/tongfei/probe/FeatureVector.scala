package me.tongfei.probe

import me.tongfei.probe.util._
import scala.collection._

/**
  * @author Tongfei Chen (ctongfei@gmail.com).
  * @since 0.4.0
  */
class FeatureVector[A] {

  private val g = mutable.HashMap[String, FeatureGroup[A]]()

  def +=(fg: FeatureGroup[A]) = {
    if (g contains fg.name) g(fg.name) = group(fg.name) + fg
    else g += (fg.name → fg)
  }

  def group(name: String) = g.getOrElse(name, FeatureGroup.empty(name))

  def groups = g.values

  def features = groups.flatMap(_.features)

  def +(that: FeatureVector[A]): FeatureVector[A] = {
    val res = new FeatureVector[A]
    this.groups foreach res.+=
    that.groups foreach res.+=
    res
  }

  def unary_- : FeatureVector[A] = {
    val res = new FeatureVector[A]
    for (g ← this.groups.map(-_)) res += g
    res
  }

  def -(that: FeatureVector[A]): FeatureVector[A] = {
    val res = new FeatureVector[A]
    for (g ← this.groups) res += g
    for (g ← that.groups.map(-_)) res += g
    res
  }

  def *(k: Double): FeatureVector[A] = {
    val res = new FeatureVector[A]
    this.groups.map(_ * k) foreach res.+=
    res
  }

  def dot(that: FeatureVector[A]): Double = {
    var sum = 0.0
    for (ga ← groups) {
      val gb = that.group(ga.name)
      for {
        (ka, va) ← ga.pairs
        (kb, vb) ← gb.pairs if ka == kb
      } sum += va * vb
    }
    sum
  }

  def l2Norm: Double = {
    var res = 0.0
    for (g ← groups)
      for ((k, v) ← g.pairs)
        res += v * v
    math.sqrt(res)
  }

  def l2Normalize = this * (1.0 / l2Norm)

  def l1Norm: Double = {
    var res = 0.0
    for (g ← groups)
      for ((k, v) ← g.pairs)
        res += math.abs(v)
    res
  }

  def maxNorm: Double = {
    var res = 0.0
    for (g ← groups)
      for ((k, v) ← g.pairs)
        res = math.max(res, v)
    res
  }

  def cosSimilarity(that: FeatureVector[A]) = (this dot that) / this.l2Norm / that.l2Norm

  def toStringFeatureVector: StringFeatureVector = {
    val sfv = new StringFeatureVector
    groups foreach sfv.<<=
    sfv
  }

  /** Returns the LIBSVM style string representation of this feature vector. */
  override def toString = groups.mkString(" ")

}

object FeatureVector {

  def apply[A](fgs: FeatureGroup[A]*): FeatureVector[A] = from(fgs)

  def from[A](fgs: Iterable[FeatureGroup[A]]): FeatureVector[A] = {
    val fv = new FeatureVector[A]
    fv.g ++= fgs.map(g => g.name → g)
    fv
  }

  /** Reads a LIBSVM style string representation of a feature vector.
    * @note The type of feature keys will be obliterated: they will be `String` in the returned vector.
    */
  def parse(s: String): FeatureVector[String] = {
    val groups = s.split(" ").map {
      case sm"$fn~$k:$v" => (fn, k, v.toDouble)
      case sm"$fn:$v" => (fn, "", v.toDouble)
    }.groupBy(_._1)
    val fv = new FeatureVector[String]
    for (g ← groups) {
      val fg = FeatureGroup(g._1) { g._2.map { case (fn, k, v) => (k, v) } }
      fv += fg
    }
    fv
  }

}