package me.tongfei.feature

import scala.collection._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class SparseVector extends mutable.HashMap[Int, Double] {
  @inline override def apply(i: Int) = getOrElse(i, 0.0)


  override def toString() = {
    this.toArray.sortBy(_._1).map{case (k, v) => s"$k:$v"}.mkString(" ")
  }
}
