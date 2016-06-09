package me.tongfei.probe

import scala.collection._

/**
 * A simple sparse vector backed by a hash map.
 * @since 0.1.0
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class SparseVector extends mutable.HashMap[Int, Double] {

  // dictates how this sparse vector behave when an absent key is accessed
  override def default(k: Int) = 0.0

  override def toString() = {
    this.toArray.sortBy(_._1).map{case (k, v) => s"$k:$v"}.mkString(" ")
  }
}
