package me.tongfei.feature

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object FeatureGroup {

  import dsl._

  def apply(featureGroup: String)(featureValues: Iterable[Any]): FeatureList = new FeatureList {
    def iterator = featureValues.iterator.map(v => featureGroup %% v $ 1.0)
  }

  def withWeight(featureGroup: String)(featureValues: Iterable[(Any, Double)]): FeatureList = new FeatureList {
    def iterator = featureValues.iterator.map(p => featureGroup %% p._1 $ p._2)
  }

}
