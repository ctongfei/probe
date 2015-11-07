package me.tongfei.probe.classifier

import de.bwaldvogel.liblinear._
import me.tongfei.probe._
import scala.collection._

/**
  * A simple log-linear model based on the library `liblinear`.
  * This is used for fast prototyping.
  * @author Tongfei Chen (ctongfei@gmail.com).
  * @since 0.4.2
  */
class LogLinearModel private(featureAlphabet: Alphabet, model: Model)
  extends (FeatureVector => Int)
{

  /** Classifies a feature vector into positive (1) or negative (0). */
  def apply(fx: FeatureVector): Int = {

    val afv = AlphabetizedFeatureVector(featureAlphabet)(fx.groups.toSeq: _*)
    val x = afv.pairs.map { t =>
      new FeatureNode(t._1, t._2).asInstanceOf[de.bwaldvogel.liblinear.Feature]
    }.toArray.sortBy(_.getIndex)
    val scores = Array.ofDim[Double](2)
    val _ = Linear.predictProbability(model, x, scores)

    if (scores(0) <= 0.5) 0 else 1
  }

  /** Returns the list of non-zero parameters (feature weights) in descending order. */
  def parameters: Iterable[(String, Double)] = {
    val w = model.getFeatureWeights
    (0 until featureAlphabet.size)
      .filter { i => w(i) != 0.0 }
      .map { i => featureAlphabet.get(i) → w(i) }
      .sortBy(-_._2)
  }

}

object LogLinearModel {

  /**
    * Fits a log-linear model using L-1 regularization.
    * @param c regularization coefficient
    * @param tol Tolerance as stopping criteria
    * @param data A sequence of training data. Each sample should be of type `(FeatureVector, Int)`.
    * @return A log-linear model
    */
  def fitWithL1Regularization(c: Double, tol: Double = 0.001)(data: Iterable[(FeatureVector, Int)]): LogLinearModel = {

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

}