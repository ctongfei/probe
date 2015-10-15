package me.tongfei.feature

/**
 * Represents a list of features extracted out of an object.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait FeatureList[+A] { self =>

  def features: Iterable[(Feature[A], Double)]

  def map[B](f: A => B): FeatureList[B] = FeatureList {
    self.features.map { case (Feature(g, a), w) => (Feature(g, f(a)), w) }
  }

  def assignWeights(f: A => Double): FeatureList[A] = FeatureList {
    self.features.map { case (Feature(g, a), w) => (Feature(g, a), f(a)) }
  }

  def compact: FeatureList[A] = FeatureList {
    self.features.groupBy(_._1) map { case (f, fs) => f → fs.map(_._2).sum }
  }

  def flatMap[B](f: A => FeatureList[B]): FeatureList[B] = FeatureList {
    for {
      (Feature(ga, va), wa) ← self.features
      (Feature(gb, vb), wb) ← f(va).features
    } yield Feature(ga + "-" + gb, vb) → (wa * wb) //TODO: Piped feature?
  }

  def filter(f: A => Boolean): FeatureList[A] = FeatureList {
    self.features.filter(p => f(p._1.value))
  }

  def topK(k: Int): FeatureList[A] = FeatureList {
    compact.features.toArray.sortBy(-_._2).take(k)
  }

  def ++[B >: A](that: FeatureList[B]): FeatureList[B] = FeatureList {
    self.features ++ that.features
  }

  def *[B](that: FeatureList[B]): FeatureList[(A, B)] = FeatureList {
    for {
      (fa, wa) ← self.features
      (fb, wb) ← that.features
    } yield ProductFeature(fa.group, fa.value, fb.group, fb.value) → (wa * wb)
  }

  def =?=[B >: A](that: FeatureList[B]): FeatureList[Unit] = FeatureList {
    for {
      (fa, wa) ← self.features
      (fb, wb) ← that.features if fa.value == fb.value
    } yield EqualityFeature(fa.group, fb.group) → (wa * wb)
  }

  /** Ignores the weight of the features and makes them all 1.0. */
  def uniformWeight = FeatureList {
    self.features.map { case (f, w) => (f, 1.0) }
  }

  /** Binarizes the feature vectors. */
  def binarize(threshold: Double) = FeatureList {
    self.features.filter(_._2 >= threshold)
  }

  //TODO: descretization

  override def toString = features.mkString("; ")
}


object FeatureList {

  def apply[A](fs: Iterable[(Feature[A], Double)]): FeatureList[A] = new FeatureList[A] {
    def features = fs
  }

//  implicit object Monad extends ConcatenativeMonad[FeatureList] {
//    def id[X](u: X): FeatureList[X] = FeatureList { Iterable(Feature("", u) → 1.0) }
//    def flatMap[X, Y](fx: FeatureList[X])(f: X => FeatureList[Y]) = fx.flatMap(f)
//    override def map[X, Y](fx: FeatureList[X])(f: X => Y) = fx.map(f)
//    override def product[X, Y](fx: FeatureList[X])(fy: FeatureList[Y]) = fx * fy
//    def empty[X]: FeatureList[X] = FeatureList { Iterable() }
//    def concat[X](f1: FeatureList[X], f2: FeatureList[X]) = f1 ++ f2
//  }

}
