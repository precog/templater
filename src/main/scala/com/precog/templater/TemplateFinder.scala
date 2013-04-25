package com.precog.templater

import scalax.file.{PathSet, Path}
import scalax.file.PathMatcher.IsFile
import scalax.file.PathMatcherFactory._

/**
 * Search for template files in the filesystem, given a set of constrains.
 *
 * @param base path used as the base for the search
 * @param suffix extension (or other string) required of returned results
 */
class TemplateFinder(base: Path, suffix: String = ".iss") {
  val suffixGlob = "*" + suffix
  /** Change the suffix searched for to a new one */
  def withSuffix(newSuffix: String): TemplateFinder = new TemplateFinder(base, suffix = newSuffix)

  /**
   * Returns all paths satisfying the constraints at the base.
   *
   * @return `PathSet` of `Path` for the files satisfying the conditions
   */
  def atBase: PathSet[Path] = {
    findFilesAt(base)
  }

  /**
   * Returns all paths satisfying the constraints at a given path relative to the base
   * or absolute.
   *
   * @param from relative path from base search path, or absolute path for the search
   * @return `PathSet` of `Path` for the files satisfying the conditions
   */
  def fromPath(from: String): PathSet[Path] = {
    val fromPath = Path fromString from
    val path = if (fromPath.isAbsolute) fromPath else (base / from)

    findFilesAt(path)
  }

  /**
   * Returns all paths satisfying the constraints at a given path.
   *
   * @param path
   * @return
   */
  def findFilesAt(path: Path): PathSet[Path] = {
    path * (IsFile && suffixGlob)
  }
}
