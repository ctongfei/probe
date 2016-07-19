package me.tongfei.probe

/**
 * Feature extractor projections.
 * @author Tongfei Chen
 * @since 0.6.2
 */
trait ProjectionOps {

  implicit class FeaturizerWithProjection[A, B](val self: Featurizer[(A, B), _]) {

    def projectFirst: Featurizer[A, _] = self match {
      case product: FeaturizerT.ProductFeaturizer[A, a, B, b] => product.f1
      case sim: FeatureSimilarityT.SimilarityFeaturizer[A, B, c] => sim.f1
      case _ => throw new FeatureProjectionException
    }

    def projectSecond: Featurizer[B, _] = self match {
      case product: FeaturizerT.ProductFeaturizer[A, a, B, b] => product.f2
      case sim: FeatureSimilarityT.SimilarityFeaturizer[A, B, c] => sim.f2
      case _ => throw new FeatureProjectionException
    }

  }

  implicit class FeatureExtractorWithProjection[A, B](val self: FeatureExtractor[(A, B), _]) {

    import FeatureExtractor._

    def projectFirst: FeatureExtractor[A, _] = self match {
      case Trivial(fz) => Trivial(fz.projectFirst)
      case Concatenated(fzs: Iterable[FeatureExtractor[(A, B), Any]]) => Concatenated(fzs map {_.projectFirst})
      case _ => throw new FeatureProjectionException
    }

    def projectSecond: FeatureExtractor[B, _] = self match {
      case Trivial(fz) => Trivial(fz.projectSecond)
      case Concatenated(fzs: Iterable[FeatureExtractor[(A, B), Any]]) => Concatenated(fzs map {_.projectSecond})
      case _ => throw new FeatureProjectionException
    }

  }

}

class FeatureProjectionException extends Exception("This feature extractor cannot be projected.")
