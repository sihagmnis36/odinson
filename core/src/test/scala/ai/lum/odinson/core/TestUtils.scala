package ai.lum.odinson.core

import java.io.File
import java.nio.file.Path

import ai.lum.odinson.ExtractorEngine
import ai.lum.common.ConfigUtils._
import com.typesafe.config.ConfigFactory

object TestUtils {
  val config             = ConfigFactory.load()
  val indexDir           = config[Path]("odinson.indexDir")

  val docIdField         = config[String]("odinson.index.documentIdField")
  val sentenceIdField    = config[String]("odinson.index.sentenceIdField")
  val wordTokenField     = config[String]("odinson.index.wordTokenField")

  val vocabFile          = config[File]("odinson.compiler.dependenciesVocabulary")

  val extractorEngine = new ExtractorEngine(indexDir)

  println("ExtractorEngine loaded w/o asplodin'")
}