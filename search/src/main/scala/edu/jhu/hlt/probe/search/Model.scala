package edu.jhu.hlt.probe.search

import java.io._

import edu.jhu.hlt.probe._
import edu.jhu.hlt.probe.classifier._

/**
 * Encapsulates a discriminative retrieval model.
 *
 * @since 0.1
 * @author Tongfei Chen
 */
class Model private(m: Map[String, Double]) {

  def parameters = m

  def apply(fqp: StringFeatureVector): Double = {
    var sum = 0.0
    for ((k, w) <- fqp)
      if (m contains k) sum += m(k) * w
    sum
  }

  def apply(fqp: FeatureVector[_]): Double = apply(fqp.toStringFeatureVector)

  def saveToFile(fn: String) = {
    val pw = new PrintWriter(fn)
    for ((k, w) <- m.toSeq.sortBy(p => -p._2))
      pw.println(s"$k\t$w")
    pw.close()
  }

}

object Model {

  /**
   * Fits a discriminative IR model based a feature extractor on pairs and a list of samples.
 *
   * @param fxy Feature extractor
   * @param xs List of samples
   * @param ys List of corresponding gold labels (should be 0 or 1 meaning irrelevant or relevant)
   */
  def fitFrom[A, B, C](fxy: FeatureExtractor[(A, B), Any], xs: Iterable[(A, B)], ys: Iterable[Int], regCoeff: Double = 1.0) = {
    val fxs = for (x <- xs) yield FeatureVector from fxy(x)
    val llm = LogLinearModel.fitWithL1Regularization(regCoeff)(fxs zip ys)
    new Model(llm.parameters.toMap)
  }

  def fromMap(m: Map[String, Double]) = new Model(m)

  def loadFromFile(fn: String): Model = {
    val m = poly.io.Local.File(fn).lines.map { case sm"$k\t$w" => k -> w.toDouble }.toMap
    fromMap(m)
  }

}
