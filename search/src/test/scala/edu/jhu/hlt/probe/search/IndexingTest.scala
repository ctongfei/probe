package edu.jhu.hlt.probe.search

import edu.jhu.hlt.probe.FeatureVector

/**
 * @author Tongfei Chen
 */

object IndexingTest extends App {

  val ib = new IndexBuilder("test-index")

  ib.add("doc1", FeatureVector parse "b:1 c:3 d:4 fff:100")
  ib.add("doc2", FeatureVector parse "a:2 c:2 b:1")
  ib.add("doc3", FeatureVector parse "f:3 d:4 a:1")
  ib.add("doc4", FeatureVector parse "b:2 e:1")

  ib.close()

}

object RetrievalTest extends App {

  val qe = new QueryEngine("test-index")

  //qe.queryWithExplanation(FeatureVector parse "a:2", 10)
  qe.queryWithExplanation(FeatureVector parse "a:10 b:0.1 fff:1", 10)
  //qe.queryWithExplanation(FeatureVector parse "a:1 b:2", 10)

}