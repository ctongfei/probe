package me.tongfei

import scala.language.implicitConversions

/**
  * @author Tongfei Chen (ctongfei@gmail.com).
  */
package object probe {

  implicit def featurizerToFeatureExtractor[A, B](f: Featurizer[A, B]): FeatureExtractor[A, B]
    = TrivialFeatureExtractor(f)

}
