package edu.jhu.hlt.probe

/**
  * @author Tongfei Chen (ctongfei@gmail.com).
  */

import edu.jhu.hlt.probe.FeatureSimilarity._

object EqualMatch extends FeatureSimilarity {
  def similarityName = "eq"
  def apply[C](fa: FeatureGroup[C], fb: FeatureGroup[C]) = {
    var sum = 0.0
    for ((_, (va, vb)) ← zipKey(fa, fb))
      sum += math.min(va, vb)
    sum
  }
}

object Dot extends FeatureSimilarity {
  def similarityName = "dot"
  def apply[C](fa: FeatureGroup[C], fb: FeatureGroup[C]) = {
    var sum = 0.0
    for ((_, (va, vb)) ← zipKey(fa, fb))
      sum += va * vb
    sum
  }
}

object CosineSimilarity extends FeatureSimilarity {
  def similarityName = "cos"
  def apply[C](fa: FeatureGroup[C], fb: FeatureGroup[C]) = {
    var ip = 0.0
    for ((_, (va, vb)) ← zipKey(fa, fb))
      ip += va * vb
    val res = ip / fa.l2Norm / fb.l2Norm
    if (res.isNaN) 0.0 else res
  }
}

object JaccardSimilarity extends FeatureSimilarity {
  def similarityName = "jaccard"
  def apply[C](fa: FeatureGroup[C], fb: FeatureGroup[C]) = {
    var minSum = 0.0
    var maxSum = 0.0
    val ma = fa.pairs.toMap
    val mb = fb.pairs.toMap
    for (k ← ma.keySet ++ mb.keySet) {
      minSum += math.min(ma.getOrElse(k, 0.0), mb.getOrElse(k, 0.0))
      maxSum += math.max(ma.getOrElse(k, 0.0), mb.getOrElse(k, 0.0))
    }
    minSum / maxSum
  }
}
