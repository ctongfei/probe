package edu.jhu.hlt.probe.search

import java.nio.file._

import edu.jhu.hlt.probe._
import org.apache.lucene.index._
import org.apache.lucene.queries.payloads._
import org.apache.lucene.queryparser.classic._
import org.apache.lucene.search._
import org.apache.lucene.search.spans._
import org.apache.lucene.store._

/**
 * Executes discriminative IR on an index.
 * @author Tongfei Chen
 */
class QueryEngine(dir: String) {

  private val directory = FSDirectory.open(Paths.get(dir))

  private val dr = DirectoryReader.open(directory)
  private val is = new IndexSearcher(dr)
  is.setSimilarity(InnerProductSimilarity)

  class InnerProductPayloadFunction(val w: Float) extends PayloadFunction {
    def docScore(docId: Int, field: String, numPayloadsSeen: Int, payloadScore: Float): Float = {
      payloadScore * w
    }

    def currentScore(docId: Int, field: String, start: Int, end: Int, numPayloadsSeen: Int, currentScore: Float, currentPayloadScore: Float): Float = {
      currentPayloadScore
    }

    override def hashCode = 0

    override def equals(obj: Any) = false
  }

  def buildQuery(fv: StringFeatureVector): Query = { //TODO: use FeatureVectorAnalyzer
    val clauses = fv.map { case (f, v) =>
      new BooleanClause(
        new PayloadScoreQuery(
          new SpanTermQuery(
            new Term(
              "content",
              f.replace(' ', '_')
            )
          ),
          new InnerProductPayloadFunction(v.toFloat)
        ),
        BooleanClause.Occur.SHOULD
      )
    }

    val qb = new BooleanQuery.Builder()
    clauses foreach qb.add
    val lq = qb.build()
    lq
  }

  private def query0(fv: StringFeatureVector, k: Int) = {
    val lq = buildQuery(fv)
    is.search(lq, k).scoreDocs.map { d =>
      is.doc(d.doc) → d.score.toDouble
    }
  }

  def query(fv: FeatureVector[_], k: Int) = query0(fv.toStringFeatureVector, k)

  private def query0WithExplanation(fv: StringFeatureVector, k: Int) = {
    val lq = buildQuery(fv)
    val res = is.search(lq, k)
    res.scoreDocs.map { d =>
      val doc = is.doc(d.doc)
      println("ID: " + doc.getField("id").stringValue())
      println("FV: " + doc.getField("content").stringValue())
      println(explain(lq, d.doc))
      is.doc(d.doc) → d.score.toDouble
    }
  }

  def queryWithExplanation(fv: FeatureVector[_], k: Int) = {
    query0WithExplanation(fv.toStringFeatureVector, k)
  }

  // FOR DEBUGGING PURPOSES
  def explain(lq: Query, docId: Int) = {
    is.explain(lq, docId)
  }

}
