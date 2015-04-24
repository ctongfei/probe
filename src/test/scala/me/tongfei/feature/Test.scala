package me.tongfei.feature

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object Test extends App {

  val fWord = FeatureGroup("word")(Array(1, 2, 3, 1, 2, 4, 6, 7))
  val f2 = Feature("abs", "word", 2.0)
  val f3 = Feature("tab", 4.0)

  val alphabet = new Alphabet
  val fv = new FeatureVector(alphabet)
  fv <<= fWord
  fv <<= f2
  fv <<= f3

  val s1 = fv.toString
  val s2 = fv.alphabetize.toString()

  val fv2 = FeatureVector.read(alphabet)(s1)
  val fv3 = FeatureVector.readAlphabetized(alphabet)(s2)

  println(s1)
  println(s2)

  println()
}
