package me.tongfei.probe

/**
 * @author Tongfei Chen
 */
object Test5 extends App {

  type Sentence = Seq[String]

  implicit val context = "Rachel eats pasta with Drew .".split(" ").toSeq

  val fxA = ContextualizedFeaturizer.count("a") { (x: Int, c: Sentence) =>
    c.slice(x - 1, x + 2)
  }

  val fxB = Featurizer.count("b") { (x: String) =>
    x.sliding(3).toSeq
  }

  println((fxA >>> fxB)(1))

}
