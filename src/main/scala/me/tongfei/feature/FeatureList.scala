package me.tongfei.feature

/**
 * A feature list is a list of features that can be iterated. Each item iterated is of type
 * [[me.tongfei.feature.Feature]], which contains the feature group, the feature value and the feature weight.
 *
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait FeatureList extends Iterable[(Feature, Double)] { self =>

  /** Concatenates two feature list. */
  def +(that: FeatureList): FeatureList = new FeatureList {
    def iterator = self.iterator ++ that.iterator
  }

  /** Returns the Cartesian product of two feature lists. */
  def *(that: FeatureList): FeatureList = new FeatureList {
    def iterator = for {
      f1 ← self.iterator
      f2 ← that.iterator
    } yield ProductFeature(f1._1, f2._1) → (f1._2 * f2._2)
  }

  /** Joins two feature lists. */
  def =*=(that: FeatureList): FeatureList = new FeatureList {
    def iterator = for {
      f1 ← self.iterator
      f2 ← that.iterator if f1._1.value == f2._1.value
    } yield JoinedFeature(f1._1, f2._1) → (f1._2 * f2._2)
  }

  /** Joins two feature lists then drop the feature value. */
  def =?=(that: FeatureList): FeatureList = new FeatureList {
    def iterator = for {
      f1 ← self.iterator
      f2 ← that.iterator if f1._1.value == f2._1.value
    } yield EqualityFeature(f1._1, f2._1) → (f1._2 * f2._2)
  }

  def toFeatureVector = StringFeatureVector(this)

  override def toString() = iterator.mkString("\n")

}

object FeatureList {

  /** Constructs an empty feature list. */
  def empty = ofIterable(Iterable())

  /** Creates a feature list based on an iterable sequence of `Feature -> Double` pairs. */
  def ofIterable(fs: Iterable[(Feature, Double)]): FeatureList = new FeatureList {
    def iterator = fs.iterator
  }

  def apply(fs: (Feature, Double)*): FeatureList = ofIterable(fs)
}
