package me.tongfei

import me.tongfei.probe._

import scala.language.implicitConversions

/**
  * @author Tongfei Chen (ctongfei@gmail.com).
  */
package object probe extends Priority0Implicits {

  implicit def featurizerToFeatureExtractor[A, B](f: Featurizer[A, B]): FeatureExtractor[A, B]
    = FeatureExtractor.Trivial(f)

  implicit class functionWithFeatureCompositionOps[A, B](val f: A => B) extends AnyVal {

    def >>>[C](g: Featurizer[B, C]) = g contramap f

    def >>>[C](g: FeatureExtractor[B, C]) = g contramap f

    def >>>[C, TC](g: FeaturizerFamily[B, C, TC]) = g contramap f

  }

}
