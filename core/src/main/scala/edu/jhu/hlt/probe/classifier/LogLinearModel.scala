package edu.jhu.hlt.probe.classifier

import de.bwaldvogel.liblinear._
import edu.jhu.hlt.probe._
import java.nio.file._
import scala.collection.JavaConversions._
import scala.collection._

/**
  * A simple log-linear model based on the library `liblinear`.
  * This is used for fast prototyping.
  * @author Tongfei Chen (ctongfei@gmail.com).
  * @since 0.4.2
  */
class LogLinearModel[A] private(featureAlphabet: Alphabet, model: Model)
  extends (FeatureVector[A] => Int)
{

  /** Classifies a feature vector into positive (1) or negative (0). */
  def apply(fx: FeatureVector[A]): Int = {
    if (score(fx) <= 0.5) 0 else 1
  }

  /** Returns P(1 | fx). */
  def score(fx: FeatureVector[A]): Double = {
    val afv = AlphabetizedFeatureVector(featureAlphabet)(fx.groups.toSeq: _*)
    val x = afv.pairs.map { t =>
      new FeatureNode(t._1, t._2).asInstanceOf[de.bwaldvogel.liblinear.Feature]
    }.toArray.sortBy(_.getIndex)
    val scores = Array.ofDim[Double](2)
    val _ = Linear.predictProbability(model, x, scores)
    scores(model.getLabels.indexOf(1))
  }

  /** Returns the list of non-zero parameters (feature weights) in descending order. */
  def parameters = {
    val w = model.getFeatureWeights
    (1 until featureAlphabet.size)
      .filter { i => w(i - 1) != 0.0 } // !!! Liblinear stores the weight for feature i in w(i - 1) !!!
      .map { i => featureAlphabet.get(i) → w(i - 1) }
      .sortBy(-_._2)
  }

  def saveToFile(fn: String) = {
    val pw = new java.io.PrintWriter(fn)
    for ((k, w) <- parameters)
      pw.println(s"$k\t$w")
    pw.close()
  }

}

object LogLinearModel {

  /**
    * Fits a log-linear model using L,,1,, regularization.
    * @param c regularization coefficient
    * @param tol Tolerance as stopping criteria
    * @param data A sequence of training data. Each sample should be of type `(FeatureVector, Int)`.
    * @return A log-linear model
    */
  def fitWithL1Regularization[A](c: Double, tol: Double = 0.001)(data: Iterable[(FeatureVector[A], Int)]): LogLinearModel[A] = {

    val featureAlphabet = new Alphabet
    val fvs = data map { _._1 }
    val fs = fvs map { fv => AlphabetizedFeatureVector(featureAlphabet)(fv.groups.toSeq: _*) }
    featureAlphabet.freeze()

    val problem = new Problem
    problem.l = fvs.size
    problem.n = featureAlphabet.size
    problem.x = fs.map { (f: AlphabetizedFeatureVector) =>
      f.pairs.map { t =>
        new FeatureNode(t._1, t._2).asInstanceOf[de.bwaldvogel.liblinear.Feature]
      }.toArray.sortBy(_.getIndex)
    }.toArray
    problem.y = data.map(_._2.toDouble).toArray

    val solver = SolverType.L1R_LR
    val parameter = new Parameter(solver, c, tol)
    val model = Linear.train(problem, parameter)

    new LogLinearModel(featureAlphabet, model)
  }

  def main(args: Array[String]) = {
    val inputFeatureFile = args(0)
    val outputModelFile = args(1)
    val regCoeff = args(2)

    val data = Files.readAllLines(Paths.get(inputFeatureFile)).map { line =>
      val spPos = line.indexOf(' ')
      val l = line.substring(0, spPos)
      val fv = line.substring(spPos + 1)
      FeatureVector.parse(fv) -> l.toInt
    }
    val model = fitWithL1Regularization(regCoeff.toDouble)(data)
    model.saveToFile(outputModelFile)
  }

}
