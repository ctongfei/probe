package me.tongfei.feature

/**
 * A featurizer is a function that takes in an object (of any type X) and returns an iterable sequence of features.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait Featurizer[X] extends (X => FeatureList) { self =>

  def +(that: Featurizer[X]) = new Featurizer[X] {
    def apply(x: X) = self(x) + that(x)
  }

  def *[Y](that: Featurizer[Y]) = new Featurizer[(X, Y)] {
    def apply(xy: (X, Y)) = self(xy._1) * that(xy._2)
  }

  def =*=[Y](that: Featurizer[Y]) = new Featurizer[(X, Y)] {
    def apply(xy: (X, Y)) = self(xy._1) =*= that(xy._2)
  }

  def =?=[Y](that: Featurizer[Y]) = new Featurizer[(X, Y)] {
    def apply(xy: (X, Y)) = self(xy._1) =?= that(xy._2)
  }

}
