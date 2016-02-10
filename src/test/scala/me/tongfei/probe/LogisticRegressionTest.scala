package me.tongfei.probe

import breeze.numerics._
import me.tongfei.probe.classifier._
import breeze.linalg._

import scala.collection._

/**
 * @author Tongfei Chen
 */
object LogisticRegressionTest extends App {

  val bow = Featurizer.count("word") { (s: String) => s.split(' ') }

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

  val alphabet = new Alphabet()
  val mapped = xs.map(_._1).map(x => AlphabetizedFeatureVector(alphabet)(x.groups.toSeq: _*))

  val lr = LogisticRegression.fit(mapped.map(_.toBreeze), xs.map(_._2), Regularizer.L2(5))

  val params = new DefaultMap[String, Double] {
    def get(key: String) = {
      val v = lr.θ(alphabet(key))
      if (abs(v) < 1e-6) Some(0) else Some(v)
    }
    def iterator = alphabet.m1.iterator.map { case (key, _) => key -> get(key).get }
  }

  val bp = 0

}
