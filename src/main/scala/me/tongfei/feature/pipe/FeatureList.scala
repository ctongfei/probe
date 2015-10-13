package me.tongfei.feature.pipe

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait FeatureList[+A] { self =>

  def features: Iterable[(Feature[A], Double)]

  def map[B](f: A => B): FeatureList[B] = FeatureList {
    self.features.map { case (Feature(g, a), w) => (Feature(g, f(a)), w) }
  }

  def flatMap[B](f: A => FeatureList[B]): FeatureList[B] = FeatureList {
    for {
      (Feature(ga, va), wa) ← self.features
      (Feature(gb, vb), wb) ← f(va).features
    } yield Feature(ga + "-" + gb, vb) → (wa * wb)
  }

  def filter(f: A => Boolean): FeatureList[A] = FeatureList {
    self.features.filter(p => f(p._1.value))
  }

  def filterWeight(f: Double => Boolean): FeatureList[A] = FeatureList {
    self.features.filter(p => f(p._2))
  }

  def topK(k: Int): FeatureList[A] = FeatureList {
    self.features.toArray.sortBy(-_._2).take(k)
  }

  def ++[B >: A](that: FeatureList[B]): FeatureList[B] = FeatureList {
    self.features ++ that.features
  }

  def *[B](that: FeatureList[B]): FeatureList[(A, B)] = FeatureList {
    for {
      (Feature(ga, va), wa) ← self.features
      (Feature(gb, vb), wb) ← that.features
    } yield Feature(ga + "," + gb, (va, vb)) → (wa * wb)
  }

  def =?=[B >: A](that: FeatureList[B]): FeatureList[Unit] = FeatureList {
    for {
      (Feature(ga, va), wa) ← self.features
      (Feature(gb, vb), wb) ← that.features if va == vb
    } yield Feature(ga + "=" + gb, ()) → (wa * wb)
  }

  def uniformWeight = FeatureList {
    self.features.map { case (f, w) => (f, 1.0) }
  }
}

object FeatureList {

  def apply[A](fs: Iterable[(Feature[A], Double)]): FeatureList[A] = new FeatureList[A] {
    def features = fs
  }

}