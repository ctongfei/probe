package me.tongfei.probe

import java.io._
import java.nio.file._
import scala.collection._
import scala.collection.JavaConversions._

/**
 * @author Tongfei Chen (tongfei@jhu.edu).
 */
class Alphabet {

  val OOV = "<OOV>"

  private var frozen = false

  val m1 = mutable.HashMap[String, Int]("<OOV>" → 0)
  val m2 = mutable.HashMap[Int, String](0 → "<OOV>")

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

  def register(w: String): Unit = {
    apply(w)
    ()
  }

  def contains(w: String): Boolean = m1 contains w

  def get(i: Int): String = m2(i)

  def size = m1.size

  def freeze() = frozen = true

  def unfreeze() = frozen = false

  def save(filename: String) = {
    val pw = new PrintWriter(filename)
    pw.write(m2.toArray.sortBy(_._1).map{case (k, v) => s"$k\t$v"}.mkString("\n"))
    pw.close()
  }

}

object Alphabet {

  def apply() = new Alphabet()

  def load(filename: String): Alphabet = {
    val alphabet = new Alphabet
    for (line ← Files.readAllLines(Paths.get(filename))) {
      val tokens = line.split("\t")
      val k = tokens(0).toInt
      val v = tokens(1)
      alphabet.m1 += v → k
      alphabet.m2 += k → v
    }
    alphabet
  }
}