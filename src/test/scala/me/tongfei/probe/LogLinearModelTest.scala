package me.tongfei.probe

import me.tongfei.probe.classifier._

/**
  * @author Tongfei Chen (ctongfei@gmail.com).
  */
object LogLinearModelTest extends App {

  val bow = Featurizer("word") { (s: String) =>
    FeatureGroup.count("word")(s.split(' '))
  }

  val fx = bow

  val xs = Seq(
    FeatureVector(fx("a a a a a a a ")) → 1,
    FeatureVector(fx("b b b b b b b ")) → 0,
    FeatureVector(fx("a a a a a d x a e a 3 q z 4")) → 1,
    FeatureVector(fx("b b 3 s 4 6 2 c d c c c c c")) → 0,
    FeatureVector(fx("c c c c a c a c c c c c c a")) → 0,
    FeatureVector(fx("b 3 d q d a a 3 b b 3 d z e")) → 1,
    FeatureVector(fx("r r z c r z c c a c c 3 z r")) → 0,
    FeatureVector(fx("c c c z d d d d d d d c c d")) → 1
  )

  val llm = LogLinearModel.fitWithL1Regularization(1)(xs)

  xs foreach { t => println(llm(t._1)) }

  llm.parameters foreach println

  val s = llm.score(xs(1)._1)

  val bp = 0

}
