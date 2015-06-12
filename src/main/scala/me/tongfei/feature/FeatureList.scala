package me.tongfei.feature

/**
 * A feature list is a list of features that can be iterated. Each item iterated is of type
 * [[me.tongfei.feature.Feature]], which contains the feature group, the feature value and the feature weight.
 *
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait FeatureList extends Iterable[Feature] { self =>

  /** Concatenates two feature list. */
  def +(that: FeatureList): FeatureList = new FeatureList {
    def iterator = self.iterator ++ that.iterator
  }

  /** Returns the Cartesian product of two feature lists. */
  def *(that: FeatureList): FeatureList = new FeatureList {
    def iterator = for {
      f1 ← self.iterator
      f2 ← that.iterator
    } yield ProductFeature(f1, f2)
  }

  /** Joins two feature lists. */
  def =*=(that: FeatureList): FeatureList = new FeatureList {
    def iterator = for {
      f1 ← self.iterator
      f2 ← that.iterator if f1.value == f2.value
    } yield JoinedFeature(f1, f2)
  }

  /** Joins two feature lists then drop the feature value. */
  def =?=(that: FeatureList): FeatureList = new FeatureList {
    def iterator = for {
      f1 ← self.iterator
      f2 ← that.iterator if f1.value == f2.value
    } yield EqualityFeature(f1, f2)
  }

  override def toString() = iterator.mkString("\n")

}

object FeatureList {

  def empty = apply(Iterable())

  def apply(fs: Iterable[Feature]): FeatureList = new FeatureList {
    def iterator = fs.iterator
  }

  def apply(fs: Feature*): FeatureList = apply(fs)
}
