package me.tongfei.feature

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object Test extends App {

  import dsl._

  val fWord = FeatureGroup("word")(Array(1, 2, 3, 1, 2, 4, 6, 7))
  val f2 = "abs" %% "word" $ 2.0
  val f3 = "tab" $ 4.0

  val alphabet = new Alphabet
  val fv = new FeatureVector(alphabet)
  fv <<= fWord
  fv <<= f2
  fv <<= f3

  val s1 = fv.toString
  val s2 = fv.alphabetize.toString()

  println(s1)
  println(s2)

  println()
}
