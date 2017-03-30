package edu.jhu.hlt.probe.search

import java.nio.file._

import edu.jhu.hlt.probe._
import org.apache.lucene.analysis.standard._
import org.apache.lucene.document._
import org.apache.lucene.index._
import org.apache.lucene.store._

/**
  * @author Tongfei Chen
  */
class IndexBuilder(dir: String) {

  val directory = FSDirectory.open(Paths.get(dir))

  val analyzer = new StandardAnalyzer()
  val iwc = new IndexWriterConfig(analyzer)
  iwc.setSimilarity(InnerProductSimilarity)
  val iw = new IndexWriter(directory, iwc)


  private def featureVectorToDocument(id: String, fp: StringFeatureVector): Document = {
    val str = fp.map { case (k, v) => k.replace(' ', '_').replace('-', '_').replace('~', '_').toLowerCase }.mkString(" ")
    val doc = new Document()
    doc.add(new StringField("id", id, Field.Store.YES))
    doc.add(new TextField("content", str, Field.Store.YES))
    doc
  }

  private def add(id: String, f: StringFeatureVector): Unit = {
    iw.addDocument(featureVectorToDocument(id, f))
  }

  def add(id: String, f: FeatureVector[_]): Unit = add(id, Util.hex(f))

  def close() = {
    iw.close()
  }

  def +=(id: String, f: FeatureVector[_]): Unit = add(id, f)

}
