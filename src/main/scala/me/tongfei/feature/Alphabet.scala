package me.tongfei.feature

import scala.collection._

/**
 * @author Tongfei Chen (tongfei@jhu.edu).
 */
class Alphabet {

  val OOV = "<OOV>"

  private var frozen = false

  private val m1 = mutable.HashMap[String, Int]("<OOV>" → 0)
  private val m2 = mutable.HashMap[Int, String](0 → "<OOV>")

  def apply(w: String): Int = {
    if (m1 contains w) m1(w)
    else if (!frozen) {
      val i = m1.size
      m1 += w → i
      m2 += i → w
      i

    }
    else 0
  }

  def contains(w: String): Boolean = m1 contains w

  def get(i: Int): String = m2(i)

  def size = m1.size

  def freeze() = frozen = true

  def unfreeze() = frozen = false

}
