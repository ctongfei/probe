package edu.jhu.hlt.probe.search

import org.apache.lucene.index._
import org.apache.lucene.search.similarities._

/**
  * This similarity function between a query and a document is computed purely via the
  * inner product of their respective sparse feature vectors.
  * It is meant to override the classic similarity measure in Lucene.
  * @author Tongfei Chen
  */
private[reflex] object InnerProductSimilarity extends ClassicSimilarity {

  override def coord(overlap: Int, maxOverlap: Int) = 1.0f

  override def idf(docFreq: Long, numDocs: Long) = 1.0f

  override def tf(freq: Float) = freq

  override def lengthNorm(state: FieldInvertState) = 1.0f

  override def queryNorm(sumOfSquares: Float) = 1.0f

}
