package edu.jhu.hlt.probe.search

import org.apache.lucene.analysis.payloads._
import org.apache.lucene.index._
import org.apache.lucene.search.similarities._
import org.apache.lucene.util._

/**
  * This similarity function between a query and a document is computed purely via the
  * inner product of their respective sparse feature vectors.
  * It is meant to override the classic similarity measure in Lucene.
  * @author Tongfei Chen
  */
object InnerProductSimilarity extends ClassicSimilarity {

  override def coord(overlap: Int, maxOverlap: Int) = 1.0f

  override def idf(docFreq: Long, numDocs: Long) = 1.0f

  override def tf(freq: Float) = freq

  override def lengthNorm(state: FieldInvertState) = 1.0f

  override def queryNorm(sumOfSquares: Float) = 1.0f

  override def scorePayload(doc: Int, start: Int, end: Int, payload: BytesRef) =
    PayloadHelper.decodeFloat(payload.bytes, payload.offset)

}
