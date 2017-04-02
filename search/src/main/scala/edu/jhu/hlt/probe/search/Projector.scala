package edu.jhu.hlt.probe.search

import edu.jhu.hlt.probe._
import edu.jhu.hlt.probe.search.parsing._

import scala.collection._

/**
 * Produces the projected vector t(q) such that t(q) · f(p) == w · f(q, p).
 *
 * @since 0.1
 * @author Tongfei Chen
 */
class Projector(m: Model) {

  type ~~>[A, B] = mutable.HashMap[A, B]

  val wJoin = mutable.HashMap[String, String ~~> Double]()
  val wProd = mutable.HashMap[String, String ~~> (String ~~> (String ~~> Double))]()

  // runs when constructor is called
  // build wJoin & wProd objects when constructing this projector
  for ((k, w) <- m.parameters) {
    val sm"$fs~$value" = k

    Util.parse(fs) match {

      case SimilarityFeatureName(sim, fq, fp) if sim == "dot" =>
        val q = fq.toString
        val p = fp.toString
        if (!(wJoin contains q)) wJoin += q -> mutable.HashMap[String, Double]()
        wJoin(q) += p -> w

      case ProductFeatureName(fq, fp) =>
        val ProductFeatureName(fqn, fpn) = Util.parse(value)
        val q = fq.toString
        val p = fp.toString
        val qn = fqn.toString
        val pn = fpn.toString
        if (!(wProd contains q)) wProd += q -> mutable.HashMap[String, String ~~> (String ~~> Double)]()
        if (!(wProd(q) contains p)) wProd(q) += p -> mutable.HashMap[String, String ~~> Double]()
        if (!(wProd(q)(p) contains qn)) wProd(q)(p) += qn -> mutable.HashMap[String, Double]()
        wProd(q)(p)(qn) += pn -> w

      case _ => throw new IllegalArgumentException(s"Feature $k is not projectable.")
    }
  }

  /**
   * Computes the vector t(q) such that t(q) · f(p) == w · f(q, p).
   */
  def project(fq: FeatureVector[_]): FeatureVector[Any] = {
    val tq = FeatureVector[Any]()
    for (g <- fq.groups) {
      if (wJoin contains g.name) {
        for ((pGroupName, pGroupWeight) <- wJoin(g.name))
          tq += g.mapValues(_ * pGroupWeight).changeName(pGroupName)
      }
      if (wProd contains g.name) {
        for ((pGroupName, pGroupWeightsMap) <- wProd(g.name)) {
          for ((qn, qw) <- g.pairs) {
            if (pGroupWeightsMap contains qn.toString)
              tq += FeatureGroup(pGroupName)(pGroupWeightsMap(qn.toString))
          }
        }
      }
    }
    tq
  }

}

object Projector {


  def fromModel(m: Model) = new Projector(m)

}
