package edu.jhu.hlt.probe.search

import edu.jhu.hlt.probe._

/**
 * @author Tongfei Chen
 */
abstract class DiscriminativeIR[Q] {

  val queryExtractor: FeatureExtractor[Q, _]

  val model: Model

  val queryEngine: QueryEngine

  lazy val projector = Projector.fromModel(model)

  def project(fq: FeatureVector[_]) = projector.project(fq)

  def query(q: Q, k: Int) = {
    val fq = FeatureVector.from(queryExtractor(q))
    val tq = projector.project(fq)
    queryEngine.query(tq, k)
  }

}
