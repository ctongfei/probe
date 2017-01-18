package edu.jhu.hlt.probe

/**
 * @author Tongfei Chen
 */

case class SingleProductSingle[A, B, C, D](self: Featurizer[A, B], that: Featurizer[C, D]) extends Featurizer[(A, C), (B, D)] {
  val name = s"(${self.name},${that.name})"
  def extract(ac: (A, C)) = {
    val (a, c) = ac
    self.extract(a) cartesianProduct that.extract(c)
  }
  override def toString = s"$self,$that"
}

case class FamilyProductSingle[A, B, TB, C, D](self: FeaturizerFamily[A, B, TB], that: Featurizer[C, D]) extends FeaturizerFamily[(A, C), (B, D), TB] {
  def name(tag: TB) = s"(${self.name(tag)},${that.name})"
  def extractWithTags(ac: (A, C)) = {
    val (a, c) = ac
    for {
      (tb, b, wb) <- self.extractWithTags(a)
      (d, wd) <- that.extract(c).pairs
    } yield (tb, (b, d), wb * wd)
  }
  override def toString = s"$self,$that"
}

case class SingleProductFamily[A, B, C, D, TD](self: Featurizer[A, B], that: FeaturizerFamily[C, D, TD]) extends FeaturizerFamily[(A, C), (B, D), TD] {
  def name(tag: TD) = s"(${self.name},${that.name(tag)})"
  def extractWithTags(ac: (A, C)) = {
    val (a, c) = ac
    for {
      (b, wb) <- self.extract(a).pairs
      (td, d, wd) <- that.extractWithTags(c)
    } yield (td, (b, d), wb * wd)
  }
  override def toString = s"$self,$that"
}

case class FamilyProductFamily[A, B, TB, C, D, TD](self: FeaturizerFamily[A, B, TB], that: FeaturizerFamily[C, D, TD]) extends FeaturizerFamily[(A, C), (B, D), (TB, TD)] {
  def name(tag: (TB, TD)) = s"(${self.name(tag._1)},${that.name(tag._2)})"
  def extractWithTags(ac: (A, C)) = {
    val (a, c) = ac
    for {
      (tb, b, wb) <- self.extractWithTags(a)
      (td, d, wd) <- that.extractWithTags(c)
    } yield ((tb, td), (b, d), wb * wd)
  }
  override def toString = s"$self,$that"
}
