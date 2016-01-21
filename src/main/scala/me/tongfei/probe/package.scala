package me.tongfei

import scala.language.implicitConversions

/**
  * @author Tongfei Chen (ctongfei@gmail.com).
  */
package object probe {

  implicit val pureContext: Unit = ()


  implicit def pureFeaturizerAsContextualized[A, B](featurizer: Featurizer[A, B]): ContextualizedFeaturizer[A, B, Any] =
    ContextualizedFeaturizer.create(featurizer.name) { (x: A, c: Any) => featurizer(x) }

  implicit def featurizerToFeatureExtractor[A, B](f: Featurizer[A, B]): FeatureExtractor[A, B]
    = FeatureExtractor.Trivial(f)

  implicit def contextualizedFeaturizerToFeatureExtractor[A, B, C](f: ContextualizedFeaturizer[A, B, C]): ContextualizedFeatureExtractor[A, B, C]
     = ContextualizedFeatureExtractor.Trivial(f)

}
