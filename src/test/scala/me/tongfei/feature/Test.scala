package me.tongfei.feature

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object Test extends App {

  val fWord = FeatureGroup("word")(Array(1, 2, 3, 1, 2, 4, 6, 7))
  val fX = FeatureGroup.withWeight("idf")((0 until 4).map(i => i → (1.0 / i)))

  val g1 = fWord * fX
  val g3 = fWord =?= fX

  val sf1 = StringFeatureVector(g1)
  val sf3 = StringFeatureVector(g3)

  val f2 = Feature("abs", "word") → 2.0
  val f3 = Feature("tab", ()) → 4.0

  val alphabet = new Alphabet
  val fv = new AlphabetizedFeatureVector(alphabet)
  fv <<= fWord
  fv <<= f2
  fv <<= f3



  val s1 = fv.toString
  val s2 = fv.internal.toString()
  val fvc = fv.copy

  val fv2 = AlphabetizedFeatureVector.read(alphabet)(s1)
  val fv3 = AlphabetizedFeatureVector.readAlphabetized(alphabet)(s2)

  println(s1)
  println(s2)

  println()
}
