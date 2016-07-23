package me.tongfei.probe

/**
 * @author Tongfei Chen
 */
object FeatureProjectionTest extends App {

  val chars = Featurizer.binary("char") { s: String => s }
  val bigrams = Featurizer.binary("bigram") { s: String => s.sliding(2).toList }

  val fi = Featurizer.binary("int") { i: Int => 0 until i }

  val fci = (chars <*> fi) ++ EqualMatch(bigrams, fi)

  val fci1 = fci.projectFirst
  val fci2 = fci.projectSecond

  val str = "abcdde"
  val int = 5

  val fci1f = FeatureVector.from(fci1(str))

  val bp = 0

}
