package com.precog.templater

import org.streum.configrity._
import scalax.io.Codec
import scalax.file.{Path, FileSystem}

/** Main class
  */

// TODO: too many configurations -- disambiguate command line configuration from template configuration
object Templater {

  def main(args: Array[String]) {
    val parser = new scopt.immutable.OptionParser[Config]("scopt", "2.x") {
      def options = Seq(
        // TODO: add codec option (really?)
        // TODO: option to limit which paramss should be processed
        opt("c", "config", "configuration file; defaults to templater.conf") {
          (v: String, c: Config) => c.copy(configPath = v)
        },
        opt("o", "output", "output directory for interpolated files; defaults to current work directory") {
          (v: String, c: Config) => c.copy(targetDir = v)
        },
        arglistOpt("<paths>", "paths to files or directories of templates") {
          (v: String, c: Config) => c.copy(sources = v :: c.sources)
        }
      )
    }

    if (args.size == 0) {
      println(parser.usage)
      sys.exit(1)
    }

    // parser.parse returns Option[C]
    parser.parse(args, Config()) foreach { argConf =>
      val conf = Configuration load argConf.configPath detach "templater" // FIXME: deal gracefully with configuration not found
      val confMultiplexer = new ConfMultiplexer
      val confMaps = confMultiplexer multiplex conf
      val targetPath = FileSystem.default fromString argConf.targetDir // FIXME: create if necessary or abort if not found

      for {
        confMap     <- confMaps
        interpolator = new Interpolator(confMap.parameters.toSeq: _*)
        source      <- argConf.sources // FIXME: only use source as base path for confMap with templateSource
      } process(interpolator, confMap, source = source, targetPath = targetPath)

      // TODO: use templateSource as source for filenames
    }
  }

  // TODO: test with ramfs
  // TODO: add timings for benchmark
  def process(interpolator: Interpolator, confMap: ConfMap, source: String, targetPath: Path) {
    // TODO: if source is a directory, treat it as a PathSet and walk through it
    // TODO: check for .iss extension
    val inputFile = Path fromString source
    val template = inputFile string Codec.UTF8
    val output = interpolator interpolate template

    val subdir = Path(confMap.path: _*)
    val outputDir = targetPath / subdir
    val outputFile = outputDir / (inputFile.simpleName)

    println(s"Generating $outputFile") // FIXME: debug logging

    outputFile.write(output)(Codec.UTF8)
  }
}
