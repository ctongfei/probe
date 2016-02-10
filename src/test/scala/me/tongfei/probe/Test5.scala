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

  val fx = (fxA >>> fxB).map(_.toLowerCase)


  println(fx(1))



  val fxC = ContextualizedFeaturizer.count("a") { (x: Int, c: String) => Iterable(x)}
  val fxD = Featurizer.count("b") {x: Int => Iterable(x)}

  val fxCD = fxC ++ fxD

}
