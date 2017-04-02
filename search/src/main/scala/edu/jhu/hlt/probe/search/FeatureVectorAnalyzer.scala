package edu.jhu.hlt.probe.search

import org.apache.lucene.analysis._
import org.apache.lucene.analysis.core._
import org.apache.lucene.analysis.payloads._

/**
 * Converts a LIBSVM format feature vector to a Lucene indexable document.
 * @author Tongfei Chen
 * @since 0.8.0
 */
class FeatureVectorAnalyzer extends Analyzer {

  val payloadEncoder = new FloatEncoder

  def createComponents(fieldName: String): Analyzer.TokenStreamComponents = {

    val source = new WhitespaceTokenizer()
    val weighted = new DelimitedPayloadTokenFilter(source, ':', payloadEncoder) // ':' being the LIBSVM format weight separator

    new Analyzer.TokenStreamComponents(source, weighted)

  }

}
