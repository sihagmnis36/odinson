package ai.lum.odinson.core

import java.nio.file.Path

import ai.lum.odinson.ExtractorEngine
import ai.lum.common.ConfigUtils._
import com.typesafe.config.ConfigFactory

object TestUtils {
  val config             = ConfigFactory.load()
  val indexDir           = config[Path]("odinson.indexDir")

  val extractorEngine = new ExtractorEngine(indexDir)
}