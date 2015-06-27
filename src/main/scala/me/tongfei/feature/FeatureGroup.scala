package me.tongfei.feature

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
object FeatureGroup {

  def apply(featureGroup: String)(featureValues: Iterable[Any]): FeatureList = new FeatureList {
    def iterator = featureValues.iterator.map(v => Feature(featureGroup, v.toString) → 1.0)
  }

  def withWeight(featureGroup: String)(featureValues: Iterable[(Any, Double)]): FeatureList = new FeatureList {
    def iterator = featureValues.iterator.map(p => Feature(featureGroup, p._1.toString) → p._2)
  }

}
