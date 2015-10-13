package me.tongfei.feature.pipe

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object FeatureGroup {

  def apply[A](group: String)(fvs: Iterable[A]) = FeatureList {
    fvs.map(v => Feature(group, v) → 1.0)
  }

  def withWeight[A](group: String)(fvs: Iterable[(A, Double)]) = FeatureList {
    fvs.map { case (v, w) => Feature(group, v) → w }
  }

}
