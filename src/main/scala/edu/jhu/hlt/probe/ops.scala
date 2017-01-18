package edu.jhu.hlt.probe

/**
 * @author Tongfei Chen
 * @since 0.6.0
 */
trait Priority0Implicits extends ProjectionOps {

  implicit class PureFeaturizerOps[A, B](val f: Featurizer[A, B]) {
    def apply(x: A) = f.extract(x)
  }

  implicit class PureFeatureExtractorOps[A, B](val f: FeatureExtractor[A, B]) {
    def apply(x: A) = f.extract(x)
  }

}
