package ai.lum.odinson.core

import ai.lum.odinson.core.TestUtils._
import org.scalatest.{FlatSpec, Matchers}

class TestQueries extends FlatSpec with Matchers {
  "ExtractorEngine" should "know the correct number of documents" in {
    extractorEngine.numDocs() should be (477) // sentences, not papers
  }

  it should "retrieve docs by ID" in {
    extractorEngine.doc(337)
    // FIXME: working .getParentDoc
    //extractorEngine.getParentDoc("10.1093/nar/gkt837")
  }

  it should "retrieve basic keyword query" in {
    val res = extractorEngine.query("phosphorylation")
    res.totalHits should be (162)
    res.maxScore should not equal 0.0
    res.scoreDocs.foreach(d => d.matches should not be empty)
    res.scoreDocs.foreach(d => d.matches.foreach{_.captures should be (empty)})
  }

  it should "only retrieve n documents" in {
    val res = extractorEngine.query("phosphorylation", 2)
    res.totalHits should be (162)
    res.scoreDocs.length should be (2)
    res.maxScore should not equal 0.0
    res.scoreDocs.foreach(d => d.matches should not be empty)
    res.scoreDocs.foreach(d => d.matches.foreach{_.captures should be (empty)})
  }

  it should "limit search by parent-doc query" in {
    val res = extractorEngine.query("phosphorylation", "title:Proteomic")
    res.scoreDocs.foreach{d =>
      extractorEngine.doc(d.doc).getField("docId").stringValue() should be ("10.1186/1559-0275-10-1")
    }
    extractorEngine.query("phosphorylation", "title:Proteomic", 1).scoreDocs.length should be (1)
  }


}