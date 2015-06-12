package me.tongfei.feature

/**
 * A feature list is a list of features that can be iterated. Each item iterated is of type `Feature`,
 * which contains the feature group, the feature value and the feature weight.
 *
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait FeatureList extends Iterable[Feature] { self =>

  def +(that: FeatureList): FeatureList = new FeatureList {
    def iterator = self.iterator ++ that.iterator
  }

  def *(that: FeatureList): FeatureList = new FeatureList {
    def iterator = for {
      f1 ← self.iterator
      f2 ← that.iterator
    } yield ProductFeature(f1, f2)
  }

  def =*=(that: FeatureList): FeatureList = new FeatureList {
    def iterator = for {
      f1 ← self.iterator
      f2 ← that.iterator if f1.value == f2.value
    } yield JoinedFeature(f1, f2)
  }

  def =?=(that: FeatureList): FeatureList = new FeatureList {
    def iterator = for {
      f1 ← self.iterator
      f2 ← that.iterator if f1.value == f2.value
    } yield EqualityFeature(f1, f2)
  }

  override def toString() = iterator.mkString("\n")

}

object FeatureList {
  def apply(fs: Iterable[Feature]): FeatureList = new FeatureList {
    def iterator = fs.iterator
  }
}