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
        arg("<path>", "path to the directory where the templates can be found") {
          (v: String, c: Config) => c.copy(source = v)
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
      val targetPath = FileSystem.default fromString argConf.targetDir
      targetPath.createDirectory(failIfExists = false, createParents = true) // TODO: Warn if exists; abort?
      val finder = new TemplateFinder(Path fromString argConf.source) // FIXME: test whether source is a directory or a file

      for {
        confMap     <- confMaps
        interpolator = new Interpolator(confMap.parameters.toSeq: _*)
        source      <- finder fromPath (confMap.source getOrElse ".")
      } process(interpolator, confMap, source = source, targetPath = targetPath)
    }
  }

  // TODO: test with ramfs
  // TODO: add timings for benchmark
  def process(interpolator: Interpolator, confMap: ConfMap, source: Path, targetPath: Path) {
    val inputFile = source
    val template = inputFile string Codec.UTF8
    val output = interpolator interpolate template

    val subdir = Path(confMap.path: _*)
    val outputDir = targetPath / subdir
    val outputFile = outputDir / (inputFile.simpleName)

    println(s"Generating $outputFile") // FIXME: debug logging

    outputFile.write(output)(Codec.UTF8)
  }
}
