package com.precog.templater

import org.specs2.Specification
import scalax.file.ramfs.RamFileSystem
import scalax.file.{PathSet, Path}

/**
  */
class TemplateFinderSpec extends Specification {

  def is = s2"""
    Template Finder
      Returns all files on a given path                 $returnsFilesFromPath
      Does not return directories on a given path       $doesntReturnDiretories
      Does not return files on subdirectories           $doesntReturnFilesFromPathSubdir
      Only returns files with default .iss suffix       $onlyReturnsFilesOfDefaultIssSuffix
      Returns files with other requested suffix         $returnsFilesWithOtherRequestedSuffix
      Returns files at paths relative to base path      $returnsFilesFromPathRelativeToBase
      Returns files from absolute path                  $returnsFilesFromAbsolutePath
      Make it possible to pass multiple subpaths?       not implemented
  """

// ramfs does not support "." and ".." at the moment
//  val basePath = fs.root
//  val fs = new RamFileSystem()

  // Test filesystem/directory
  val basePath = Path.createTempDirectory(deleteOnExit = true)
  val absolutePath = Path.createTempDirectory(deleteOnExit = true)
  val finder = new TemplateFinder(basePath)
  val rootTemplateFiles = Seq("a.iss", "b.iss", "c.iss")
  val rootTextFiles = Seq("x.txt")
  val rootErbFiles = Seq("y.erb")
  val rootNonTemplateFiles = rootTextFiles ++ rootErbFiles
  val rootFiles = rootTemplateFiles ++ rootNonTemplateFiles
  val rootDirectories = Seq("subdir.iss", "other")
  val otherTemplateFiles = Seq("d.iss", "e.iss")
  val absoluteTemplateFiles = rootTemplateFiles ++ otherTemplateFiles

  // Initialize fs with contents
  rootFiles foreach (basePath / _ createFile())
  rootDirectories foreach (basePath / _ createDirectory())
  otherTemplateFiles foreach (basePath / "other" / _ createFile())
  absoluteTemplateFiles foreach (absolutePath / _ createFile())

  // Helper method
  def asListOfFileNames(ps: PathSet[Path]): List[String] = ps.toList map (_.name)

  // Tests
  def returnsFilesFromPath = {
    asListOfFileNames(finder fromPath ".") must containAllOf(rootTemplateFiles)
  }

  def doesntReturnDiretories = {
    asListOfFileNames(finder fromPath ".") must not(containAnyOf(rootDirectories))
  }

  def onlyReturnsFilesOfDefaultIssSuffix = {
    asListOfFileNames(finder fromPath ".") must not(containAnyOf(rootNonTemplateFiles))
  }

  def returnsFilesWithOtherRequestedSuffix = {
    asListOfFileNames(finder withSuffix ".txt" fromPath ".") must containAllOf(rootTextFiles)
  }

  def doesntReturnFilesFromPathSubdir = {
    asListOfFileNames(finder fromPath ".") must not(containAnyOf(otherTemplateFiles))
  }

  def returnsFilesFromPathRelativeToBase = {
    asListOfFileNames(finder fromPath "other") must containAllOf(otherTemplateFiles)
  }

  def returnsFilesFromAbsolutePath = {
    asListOfFileNames(finder fromPath absolutePath.path) must containAllOf(absoluteTemplateFiles)
  }
}
