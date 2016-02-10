package me.tongfei.probe.classifier

import breeze.linalg._
import breeze.numerics._
import breeze.optimize._


/**
 * @author Tongfei Chen
 */
class LogisticRegression(n: Int) extends (Vector[Double] => Int) {

  val θ: Vector[Double] = DenseVector.zeros(n)

  def score(x: Vector[Double]) = 1.0 / (1 + exp(-(θ.t * x)))

  def apply(x: Vector[Double]) = if (score(x) > 0.5) 1 else 0

}


object LogisticRegression {


  def fit(
           xs: Seq[Vector[Double]],
           ys: Seq[Int],
           regularizer: Regularizer,
           maxIterations: Int = 100,
           iterationCallback: ((Int, LogisticRegression) => Any) = (_, _) => {}
         ): LogisticRegression = {

    val N = xs.length
    val n = xs.head.length

    val lr = new LogisticRegression(n)

    val observed = DenseVector.zeros[Double](n)
    for (i ← 0 until N if ys(i) == 1) observed += xs(i)

    def predicted(θ: Vector[Double]) = {
      val p = DenseVector.zeros[Double](n)
      for (i ← 0 until N) p += xs(i) :* Loss.Log.score(xs(i), θ)
      p
    }

    val lbfgs = new LBFGS[Vector[Double]]()
    val objective = new DiffFunction[Vector[Double]] {
      def calculate(θ: Vector[Double]) = (
        Loss.Log(xs zip ys)(θ) + regularizer(θ),  // loss function
        -observed + predicted(θ) + regularizer.derivative(θ)   // derivative of loss function
      )
    }

    val param = lbfgs.minimize(objective, lr.θ)

    lr.θ := param
    lr
  }

}