package me.tongfei.probe

import me.tongfei.probe.util._
import scala.collection._

/**
  * @author Tongfei Chen (ctongfei@gmail.com).
  * @since 0.4.0
  */
class FeatureVector {

  private val g = mutable.HashMap[String, FeatureGroup[Any]]()

  def +=(fg: FeatureGroup[Any]) = {
    if (g contains fg.name) g(fg.name) = group(fg.name) + fg
    else g += (fg.name → fg)
  }

  def group(name: String) = g.getOrElse(name, FeatureGroup.empty(name))

  def groups = g.values

  def features = groups.flatMap(_.features)

  def +(that: FeatureVector): FeatureVector = {
    val res = new FeatureVector
    this.groups foreach res.+=
    that.groups foreach res.+=
    res
  }

  def unary_- : FeatureVector = {
    val res = new FeatureVector
    for (g ← this.groups.map(-_)) res += g
    res
  }

  def -(that: FeatureVector): FeatureVector = {
    val res = new FeatureVector
    for (g ← this.groups) res += g
    for (g ← that.groups.map(-_)) res += g
    res
  }

  def *(k: Double): FeatureVector = {
    val res = new FeatureVector
    this.groups.map(_ * k) foreach res.+=
    res
  }

  def dot(that: FeatureVector): Double = {
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

  def cosSimilarity(that: FeatureVector) = (this dot that) / this.l2Norm / that.l2Norm

  def toStringFeatureVector: StringFeatureVector = {
    val sfv = new StringFeatureVector
    groups foreach sfv.<<=
    sfv
  }

  /** Returns the LIBSVM style string representation of this feature vector. */
  override def toString = groups.mkString(" ")

}

object FeatureVector {

  def apply(fgs: FeatureGroup[Any]*): FeatureVector = apply(fgs)

  def apply(fgs: Iterable[FeatureGroup[Any]]): FeatureVector = {
    val fv = new FeatureVector
    fv.g ++= fgs.map(g => g.name → g)
    fv
  }

  /** Reads a LIBSVM style string representation of a feature vector.
    * @note The type of feature keys will be obliterated: they will be `String` in the returned vector.
    */
  def parse(s: String) = {
    val groups = s.split(" ").map {
      case sm"$fn~$k:$v" => (fn, k, v.toDouble)
      case sm"$fn:$v" => (fn, (), v.toDouble)
    }.groupBy(_._1)
    val fv = new FeatureVector
    for (g ← groups) {
      val fg = FeatureGroup(g._1) { g._2.map { case (fn, k, v) => (k, v) } }
      fv += fg
    }
    fv
  }

}