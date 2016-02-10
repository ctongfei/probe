package me.tongfei.probe.classifier

import breeze.linalg._
import breeze.numerics._

/**
 * @author Tongfei Chen
 */
trait Regularizer {

  def apply(w: Vector[Double]): Double

  def derivative(w: Vector[Double]): Vector[Double]

}

object Regularizer {

  object None extends Regularizer {
    def apply(w: Vector[Double]) = 0.0
    def derivative(w: Vector[Double]) = DenseVector.zeros[Double](w.length)
  }

  def L1(λ: Double): Regularizer = new Regularizer {
    def derivative(w: Vector[Double]) = signum(w) :* λ
    def apply(w: Vector[Double]) = sum(abs(w)) * λ
  }

  def L2(λ: Double): Regularizer = new Regularizer {
    def derivative(w: Vector[Double]) = w :* λ
    def apply(w: Vector[Double]) = norm(w) * norm(w) * 0.5
  }

}
