package ai.lum.odinson

import java.nio.file.Path
import org.apache.lucene.analysis.core.WhitespaceAnalyzer
import org.apache.lucene.document.{ Document => LuceneDocument }
import org.apache.lucene.search.{ BooleanClause => LuceneBooleanClause, BooleanQuery => LuceneBooleanQuery }
import org.apache.lucene.store.{ Directory, FSDirectory }
import org.apache.lucene.index.DirectoryReader
import org.apache.lucene.queryparser.classic.QueryParser
import com.typesafe.config.Config
import ai.lum.common.ConfigUtils._
import ai.lum.common.StringUtils._
import ai.lum.common.ConfigFactory
import ai.lum.odinson.compiler.QueryCompiler
import ai.lum.odinson.lucene._
import ai.lum.odinson.lucene.analysis.TokenStreamUtils
import ai.lum.odinson.lucene.search._
import ai.lum.odinson.state.State
import ai.lum.odinson.digraph.Vocabulary



class ExtractorEngine(
  val indexSearcher: OdinsonIndexSearcher,
  val compiler: QueryCompiler,
  val displayField: String,
  val state: State,
  val parentDocIdField: String
) {

  /** Analyzer for parent queries.  Don't skip any stopwords. */
  val analyzer = new WhitespaceAnalyzer()

  val indexReader = indexSearcher.getIndexReader()

  def doc(docID: Int): LuceneDocument = {
    indexSearcher.doc(docID)
  }

  def numDocs(): Int = {
    indexReader.numDocs()
  }

  /** Retrieves the parent Lucene Document by docId */
  def getParentDoc(docId: String): LuceneDocument = {
    val sterileDocID =  docId.escapeJava
    val booleanQuery = new LuceneBooleanQuery.Builder()
    val q1 = new QueryParser(parentDocIdField, analyzer).parse(s""""$sterileDocID"""")
    booleanQuery.add(q1, LuceneBooleanClause.Occur.MUST)
    val q2 = new QueryParser("type", analyzer).parse("root")
    booleanQuery.add(q2, LuceneBooleanClause.Occur.MUST)
    val q = booleanQuery.build
    val docs = indexSearcher.search(q, 10).scoreDocs.map(sd => indexReader.document(sd.doc))
    //require(docs.size == 1, s"There should be only one parent doc for a docId, but ${docs.size} found.")
    docs.head
  }

  def explain(d: OdinsonScoreDoc, m: OdinsonMatch): Option[Explanation] = {
    m match {
      case m: GraphTraversalMatch =>
        val tokens = getTokens(d)
        val vocab = compiler.dependenciesVocabulary
        var path = ""
        var lexPath = ""
        var first = true
        for (step <- m.traversedPath.path) {
          val from = maybeQuoteWord(tokens(step.from))
          val to = maybeQuoteWord(tokens(step.to))
          val label = maybeQuoteLabel(vocab.getTerm(step.edgeLabel).get)
          val direction = if (step.direction == "incoming") "<" else ">"
          if (first) {
            path += s"$direction$label"
            lexPath += s"$direction$label"
            first = false
          } else {
            path += s" $direction$label"
            lexPath += s" $from $direction$label"
          }
        }
        val sentence = tokens.mkString(" ")
        val src = tokens.slice(m.srcMatch.start, m.srcMatch.end).mkString(" ")
        val dst = tokens.slice(m.dstMatch.start, m.dstMatch.end).mkString(" ")
        val explanation = new Explanation(sentence, src, dst, path, lexPath)
        Some(explanation)
      case _ => None
    }
  }

  // FIXME move this method to somewhere else
  // utils maybe?
  def maybeQuoteWord(s: String): String = {
    val isValidIdentifier = "^[a-zA-Z_][a-zA-Z0-9_]*$".r.findFirstIn(s).isDefined
    if (isValidIdentifier) s else "\"" + s.escapeJava + "\""
  }

  def maybeQuoteLabel(s: String): String = {
    val isValidIdentifier = "^[a-zA-Z_][a-zA-Z0-9_:-]*$".r.findFirstIn(s).isDefined
    if (isValidIdentifier) s else "\"" + s.escapeJava + "\""
  }

  /** executes query and returns all results */
  def query(odinsonQuery: String): OdinResults = {
    query(odinsonQuery, indexReader.numDocs())
  }

  /** executes query and returns at most n documents */
  def query(odinsonQuery: String, n: Int): OdinResults = {
    query(compiler.mkQuery(odinsonQuery), n)
  }

  /** executes query and returns at most n documents */
  def query(odinsonQuery: String, parentQuery: String): OdinResults = {
    query(odinsonQuery, parentQuery, indexReader.numDocs())
  }

  /** executes query and returns at most n documents */
  def query(odinsonQuery: String, parentQuery: String, n: Int): OdinResults = {
    query(compiler.mkQuery(odinsonQuery, parentQuery), n)
  }

  /** executes query and returns all results */
  def query(odinsonQuery: OdinsonQuery): OdinResults = {
    query(odinsonQuery, indexReader.numDocs())
  }

  /** executes query and returns at most n documents */
  def query(odinsonQuery: OdinsonQuery, n: Int): OdinResults = {
    indexSearcher.odinSearch(odinsonQuery, n)
  }

  /** executes query and returns next n results after the provided doc */
  def query(
    odinsonQuery: String,
    n: Int,
    afterDoc: Int,
    afterScore: Float
  ): OdinResults = {
    query(
      compiler.mkQuery(odinsonQuery),
      n,
      new OdinsonScoreDoc(afterDoc, afterScore)
    )
  }

  /** executes query and returns next n results after the provided doc */
  def query(
    odinsonQuery: String,
    parentQuery: String,
    n: Int,
    afterDoc: Int,
    afterScore: Float
  ): OdinResults = {
    query(
      compiler.mkQuery(odinsonQuery, parentQuery),
      n,
      new OdinsonScoreDoc(afterDoc, afterScore)
    )
  }

  /** executes query and returns next n results after the provided doc */
  def query(
    odinsonQuery: String,
    n: Int,
    after: OdinsonScoreDoc
  ): OdinResults = {
    query(compiler.mkQuery(odinsonQuery), n, after)
  }

  /** executes query and returns next n results after the provided doc */
  def query(
    odinsonQuery: String,
    parentQuery: String,
    n: Int,
    after: OdinsonScoreDoc
  ): OdinResults = {
    query(compiler.mkQuery(odinsonQuery, parentQuery), n, after)
  }

  /** executes query and returns next n results after the provided doc */
  def query(
    odinsonQuery: OdinsonQuery,
    n: Int,
    after: OdinsonScoreDoc
  ): OdinResults = {
    indexSearcher.odinSearch(after, odinsonQuery, n)
  }

  def getString(docID: Int, m: OdinsonMatch): String = {
    getTokens(docID, m).mkString(" ")
  }

  def getTokens(docID: Int, m: OdinsonMatch): Array[String] = {
    getTokens(docID, displayField).slice(m.start, m.end)
  }

  def getTokens(scoreDoc: OdinsonScoreDoc): Array[String] = {
    getTokens(scoreDoc.doc, displayField)
  }

  def getTokens(scoreDoc: OdinsonScoreDoc, fieldName: String): Array[String] = {
    getTokens(scoreDoc.doc, fieldName)
  }

  def getTokens(docID: Int, fieldName: String): Array[String] = {
    TokenStreamUtils.getTokens(docID, fieldName, indexSearcher, analyzer)
  }

}

object ExtractorEngine {

  def fromConfig(): ExtractorEngine = {
    fromConfig("odinson")
  }

  def fromConfig(path: String): ExtractorEngine = {
    val config = ConfigFactory.load()
    fromConfig(config[Config](path))
  }

  def fromConfig(config: Config): ExtractorEngine = {
    val indexPath = config[Path]("indexDir")
    val indexDir = FSDirectory.open(indexPath)
    fromDirectory(config, indexDir)
  }

  def fromDirectory(config: Config, indexDir: Directory): ExtractorEngine = {
    val indexReader = DirectoryReader.open(indexDir)
    val computeTotalHits = config[Boolean]("computeTotalHits")
    val displayField = config[String]("displayField")
    val indexSearcher = new OdinsonIndexSearcher(indexReader, computeTotalHits)
    val vocabulary = Vocabulary.fromDirectory(indexDir)
    val compiler = QueryCompiler(config, vocabulary)
    val jdbcUrl = config[String]("state.jdbc.url")
    val state = new State(jdbcUrl)
    state.init()
    compiler.setState(state)
    val parentDocIdField = config[String]("index.documentIdField")
    new ExtractorEngine(
      indexSearcher,
      compiler,
      displayField,
      state,
      parentDocIdField
    )
  }

}
