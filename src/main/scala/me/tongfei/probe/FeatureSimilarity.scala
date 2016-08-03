package me.tongfei.probe

import scala.collection.mutable

/**
  * Represents a similarity function that when given two feature groups,
  * produces in a single similarity feature.
 *
 * @author Tongfei Chen (ctongfei@gmail.com).
  * @since 0.4.1
  */
trait FeatureSimilarity { self =>

  def similarityName: String

  /** Given two feature groups, computes their similarity measure. */
  def apply[C](fa: FeatureGroup[C], fb: FeatureGroup[C]): Double

  /** Given two featurizers, produces a featurizer that returns a single feature that contains their similarity. */
  def apply[A, B, C](f1: Featurizer[A, C], f2: Featurizer[B, C]): Featurizer[(A, B), Unit] =
    new FeatureSimilarityT.SimilarityFeaturizer(self, f1, f2)

  def apply[A, B](f: Featurizer[A, B]): Featurizer[(A, A), Unit] = apply(f, f)

  def apply[A, B, C, T1, T2](f1: FeaturizerFamily[A, C, T1], f2: FeaturizerFamily[B, C, T2]): FeaturizerFamily[(A, B), Unit, (T1, T2)] =
    new FeatureSimilarityT.SimilarityFeaturizerFamily(self, f1, f2)

}

object FeatureSimilarity {

  def zipKey[C](fa: FeatureGroup[C], fb: FeatureGroup[C]): Iterable[(C, (Double, Double))] = {
    val mb = fb.pairs.toMap
    for {
      (ka, va) ← fa.pairs if mb contains ka
    } yield ka → (va, mb(ka))
  }

}

private[tongfei] object FeatureSimilarityT {

  case class SimilarityFeaturizer[A, B, C](sim: FeatureSimilarity, f1: Featurizer[A, C], f2: Featurizer[B, C]) extends Featurizer[(A, B), Unit] {
    val name = s"${sim.similarityName}(${f1.name},${f2.name})"
    def extract(pair: (A, B)) = {
      val (a, b) = pair
      val fa = f1.extract(a)
      val fb = f2.extract(b)
      SingleNumericalFeature(name)(sim(fa, fb))
    }
  }

  case class SimilarityFeaturizerFamily[A, B, C, T1, T2](sim: FeatureSimilarity, f1: FeaturizerFamily[A, C, T1], f2: FeaturizerFamily[B, C, T2]) extends FeaturizerFamily[(A, B), Unit, (T1, T2)] {
    def name(tag: (T1, T2)) = s"${sim.similarityName}(${f1.name(tag._1)},${f2.name(tag._2)})"
    def extractWithTags(x: (A, B)) = {
      val (a, b) = x

      val ma = mutable.HashMap[T1, mutable.ArrayBuffer[(C, Double)]]()
      for ((t1, c, w) <- f1.extractWithTags(a)) {
        if (!ma.contains(t1)) ma += t1 -> mutable.ArrayBuffer[(C, Double)]()
        ma(t1) += c -> w
      }
      val mb = mutable.HashMap[T2, mutable.ArrayBuffer[(C, Double)]]()
      for ((t2, c, w) <- f2.extractWithTags(b)) {
        if (!mb.contains(t2)) mb += t2 -> mutable.ArrayBuffer[(C, Double)]()
        mb(t2) += c -> w
      }

      for {
        (ta, fas) <- ma
        (tb, fbs) <- mb
      } yield ((ta, tb), (), sim(FeatureGroup(f1.name(ta))(fas), FeatureGroup(f2.name(tb))(fbs)))
    }
  }

}