package me.tongfei.probe.classifier

import breeze.linalg._
import breeze.numerics._

/**
 * @author Tongfei Chen
 */
trait Loss[-T, -P] {

  def apply(xs: Seq[T])(params: P): Double

}

object Loss {

  object Log extends Loss[(Vector[Double], Int), Vector[Double]] {

    def score(x: Vector[Double], θ: Vector[Double]) = 1.0 / (1 + exp(-(θ.t * x)))

    def apply(xs: Seq[(Vector[Double], Int)])(θ: Vector[Double]) = {
      var sum = 0.0
      for ((x, y) ← xs) {
        if (y == 1) sum += log(score(x, θ))
        else sum += log(1 - score(x, θ))
      }
      -sum
    }
  }

}
