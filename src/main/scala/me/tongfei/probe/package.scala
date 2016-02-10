package me.tongfei

import me.tongfei.probe._

import scala.language.implicitConversions

/**
  * @author Tongfei Chen (ctongfei@gmail.com).
  */
package object probe extends Priority0Implicits {

  implicit def featurizerToFeatureExtractor[A, B](f: Featurizer[A, B]): FeatureExtractor[A, B]
    = FeatureExtractor.Trivial(f)

  implicit def contextualizedFeaturizerToFeatureExtractor[A, B, C](f: ContextualizedFeaturizer[A, B, C]): ContextualizedFeatureExtractor[A, B, C]
    = ContextualizedFeatureExtractor.Trivial(f)

}
