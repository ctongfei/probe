package me.tongfei.probe

/**
 * @author Tongfei Chen
 * @since 0.6.0
 */
trait Priority0Implicits extends Priority1Implicits {

  implicit class PureFeaturizerOps[A, B](val f: Featurizer[A, B]) {
    def apply(x: A) = f.extract(x)
  }

  implicit class PureFeatureExtractorOps[A, B](val f: FeatureExtractor[A, B]) {
    def apply(x: A) = f.extract(x)
  }

}

trait Priority1Implicits {

  implicit class ContextualizedFeaturizerOps[A, B, C](val f: ContextualizedFeaturizer[A, B, C]) {
    def apply(a: A)(implicit c: C) = f.extract(a, c)
  }

  implicit class ContextualizedFeatureExtractorOps[A, B, C](val f: ContextualizedFeatureExtractor[A, B, C]) {
    def apply(a: A)(implicit c: C) = f.extract(a, c)
  }

}