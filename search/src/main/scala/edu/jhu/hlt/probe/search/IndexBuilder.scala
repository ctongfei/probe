package edu.jhu.hlt.probe.search

import java.nio.file._

import edu.jhu.hlt.probe._
import org.apache.lucene.document._
import org.apache.lucene.index._
import org.apache.lucene.store._

/**
  * @author Tongfei Chen
  */
class IndexBuilder(dir: String) {

  poly.io.Local.Directory(dir).clear()
  val directory = FSDirectory.open(Paths.get(dir))

  val analyzer = new FeatureVectorAnalyzer()
  val iwc = new IndexWriterConfig(analyzer)
  iwc.setSimilarity(InnerProductSimilarity)
  val iw = new IndexWriter(directory, iwc)

  private def featureVectorToDocument(id: String, fp: StringFeatureVector): Document = {
    val str = fp.toString() // directly uses the LIBSVM string itself: will be properly analyzed by [[FeatureVectorAnalyzer]]
    val doc = new Document()
    doc.add(new StringField("id", id, Field.Store.YES))
    doc.add(new TextField("content", str, Field.Store.YES))
    doc
  }

  private def add(id: String, f: StringFeatureVector): Unit = {
    iw.addDocument(featureVectorToDocument(id, f))
  }

  def add(id: String, f: FeatureVector[_]): Unit = {
    add(id, f.toStringFeatureVector)
  }

  def close() = {
    iw.close()
  }

}
