package ai.lum.odinson.core

import ai.lum.odinson.core.TestUtils._
import org.scalatest.{FlatSpec, Matchers}

class TestQueries extends FlatSpec with Matchers {
  "ExtractorEngine" should "know the correct number of documents" in {
    extractorEngine.numDocs() should be (477) // sentences, not papers
  }

  it should "retrieve docs by ID" in {
    extractorEngine.doc(0)
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
    res.scoreDocs.foreach{ d =>
      extractorEngine.doc(d.doc).getField("docId").stringValue() should be ("10.1186/1559-0275-10-1")
    }
    extractorEngine.query("phosphorylation", "title:Proteomic", 1).scoreDocs.length should be (1)
  }

  it should "find mandatory capture groups" in {
    val res = extractorEngine.query("(?<trigger> phosphorylation) >nmod_of (?<theme> [])")
    res.scoreDocs should not be empty
    res.scoreDocs.foreach{d =>
      d.matches.foreach{ swc =>
        swc.captures.map(_._1) should contain ("trigger")
        swc.captures.map(_._1) should contain ("theme")
      }
    }
  }

  it should "limit spans by single-parameter quantification" in {
    val res = extractorEngine.query("(?<trigger> phosphorylation) >nmod_of (?<theme> [tag=NN]{2})")
    res.scoreDocs should not be empty
    val themes = res.scoreDocs.flatMap(_.matches.flatMap(_.captures.filter(_._1 == "theme").map(_._2)))
    themes.foreach(s => s.interval.length should be (2))
  }

  it should "limit spans by quantification range with specified endpoints" in {
    val res = extractorEngine.query("(?<trigger> phosphorylation) >nmod_of (?<theme> [tag=NN]{2, 3})")
    res.scoreDocs should not be empty
    val themes = res.scoreDocs.flatMap(_.matches.flatMap(_.captures.filter(_._1 == "theme").map(_._2.interval.length)))
    themes.foreach{ l =>
      l should be >= 2
      l should be <= 3
    }
    themes should contain (2)
    themes should contain (3)
  }

  it should "limit spans by quantification range with a lower limit specified" in {
    val res = extractorEngine.query("(?<trigger> phosphorylation) >nmod_of (?<theme> [tag=NN]{, 3})")
    res.scoreDocs should not be empty
    val themes = res.scoreDocs.flatMap(_.matches.flatMap(_.captures.filter(_._1 == "theme").map(_._2.interval.length)))
    themes.foreach{ l =>
      l should be <= 3
    }
    themes should contain (1)
    themes should contain (2)
    themes should contain (3)
  }

  it should "limit spans by quantification range with an upper limit specified" in {
    val res = extractorEngine.query("(?<trigger> phosphorylation) >nmod_of (?<theme> [tag=NN]{2, })")
    res.scoreDocs should not be empty
    val themes = res.scoreDocs.flatMap(_.matches.flatMap(_.captures.filter(_._1 == "theme").map(_._2.interval.length)))
    themes.foreach{ l =>
      l should be >= 2
    }
    themes should contain (2)
    themes should contain (3)
  }


}