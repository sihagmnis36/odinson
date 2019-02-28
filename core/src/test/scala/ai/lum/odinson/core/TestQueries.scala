package ai.lum.odinson.core

import ai.lum.odinson.core.TestUtils._
import org.scalatest.{FlatSpec, Matchers}

class TestQueries extends FlatSpec with Matchers {
  "ExtractorEngine" should "know the correct number of documents" in {
    extractorEngine.numDocs() should be (477) // sentences, not papers
  }

  it should "" in {

  }
}