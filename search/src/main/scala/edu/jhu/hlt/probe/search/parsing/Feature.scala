package edu.jhu.hlt.probe.search.parsing

import fastparse.all._

/**
 * Principled feature name parsing using a CFG.
 * @author Tongfei Chen
 * @since 0.2.1
 */
sealed trait FeatureName

case class SimpleFeatureName(s: String) extends FeatureName {
  override def toString = s
}

case class ProductFeatureName(s: FeatureName, t: FeatureName) extends FeatureName {
  override def toString = s"($s,$t)"
}

case class SimilarityFeatureName(sim: String, s: FeatureName, t: FeatureName) extends FeatureName {
  override def toString = s"$sim($s,$t)"
}

object FeatureParsing {

  lazy val S: Parser[FeatureName] = (Simple ~ End) | (Product ~ End) | (Similarity ~ End)

  lazy val Any: Parser[FeatureName] = Simple | Product | Similarity

  val identifierChars = (('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9') ++ "._-").toSet

  val Identifier = P { CharsWhile(identifierChars).! }

  val Simple = Identifier map { s => SimpleFeatureName(s) }

  val Product = P { "(" ~ Any ~ "," ~ Any ~ ")" } map { case (s, t) => ProductFeatureName(s, t) }

  val Similarity = P { Identifier ~ Product } map { case (sim, ProductFeatureName(s, t)) => SimilarityFeatureName(sim, s, t) }

}
