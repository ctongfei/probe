package edu.jhu.hlt.probe.search

import java.nio.file._

import edu.jhu.hlt.probe._
import org.apache.lucene.index._
import org.apache.lucene.search._
import org.apache.lucene.store._

/**
  * @author Tongfei Chen
  */

class QueryEngine(dir: String) {

  val directory = FSDirectory.open(Paths.get(dir))

  val dr = DirectoryReader.open(directory)
  val is = new IndexSearcher(dr)
  is.setSimilarity(InnerProductSimilarity)

  def buildQuery(fv: StringFeatureVector): Query = {
    val clauses = fv.map { case (f, v) =>
      new BooleanClause(
        new BoostQuery(
          new TermQuery(
            new Term(
              "content",
              f.replace(' ', '_').replace('-', '_').replace('~', '_').toLowerCase
            )
          ),
          v.toFloat
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
      is.doc(d.doc) → d.score
    }
  }

  def query(fv: FeatureVector[_], k: Int) = {
    query0(Util.hex(fv), k)
  }

  private def query0WithExplanation(fv: StringFeatureVector, k: Int) = {
    val lq = buildQuery(fv)
    is.search(lq, k).scoreDocs.map { d =>
      println(is.doc(d.doc).getField("content").stringValue())
      println(explain(lq, d.doc))
      is.doc(d.doc) → d.score
    }
  }

  def queryWithExplanation(fv: FeatureVector[_], k: Int) = {
    query0WithExplanation(Util.hex(fv), k)
  }

  // FOR DEBUGGING PURPOSES
  def explain(lq: Query, docId: Int) = {
    is.explain(lq, docId)
  }

}
