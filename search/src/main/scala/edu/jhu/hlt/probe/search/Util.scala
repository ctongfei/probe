package edu.jhu.hlt.probe.search

import java.math._

import edu.jhu.hlt.probe._
import edu.jhu.hlt.probe.search.parsing._

/**
 * @author Tongfei Chen
 */
object Util {

  def hex(s: Any): String = String.format("%x", new BigInteger(1, s.toString.getBytes))

  def hex(fv: FeatureVector[_]): StringFeatureVector = {
    val sfv = new StringFeatureVector
    for (g <- fv.groups)
      sfv <<= g map hex
    sfv
  }

  def parse(s: String): FeatureName = parsing.FeatureParsing.S.parse(s).get.value

}
