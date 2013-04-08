package com.precog.templater

import org.specs2.Specification
import org.streum.configrity.Configuration
import org.streum.configrity.io.BlockFormat

/**
  */
class ConfMultiplexerSpec extends Specification {

  def is = s2"""
    Flat configurations return all values in a single list          $flatConfigurationIsOneList
    Strings are quoted, booleans and integers are not               $quoteStringButNotIntsAndBooleans
    Lists are decoded as lists whose string values are quoted       $listsAreListsOfValues
    Nested blocks are ignored                                       $ignoreNestedBlocksTest
  """

  val confMultiplexer = new ConfMultiplexer

  val sourceConfiguration =
    """
      |templater {
      |  x = 5
      |  y = true
      |  z = abc
      |  xx = [1, 2, 3]
      |  yy = [true, false]
      |  zz = [abc, def]
      |}
    """.stripMargin

  val conf = Configuration parse (sourceConfiguration, BlockFormat) detach "templater"

  def flatConfigurationIsOneList = {
    val multiplexedValues @ (values :: _) = confMultiplexer multiplex conf
    (multiplexedValues must haveSize(1)) &&
    (values must haveSize(6))
  }

  def quoteStringButNotIntsAndBooleans = {
    val values :: _ = confMultiplexer multiplex conf
    values must haveAllElementsLike {
      case ("x", xValue) => xValue === "5"
      case ("y", yValue) => yValue === "true"
      case ("z", zValue) => zValue === "\"abc\""
    }
  }

  def listsAreListsOfValues = {
    val values :: _ = confMultiplexer multiplex conf
    values must haveAllElementsLike {
      case ("xx", xValue) => xValue === "List(1, 2, 3)"
      case ("yy", yValue) => yValue === "List(true, false)"
      case ("zz", zValue) => zValue === """List("abc", "def")"""
    }
  }

  def ignoreNestedBlocksTest = {
    val nestedBlock =
      """
        |  a = 5
        |  b = 3
      """.stripMargin

    val nestedConf = conf attach ("subBlock", Configuration parse nestedBlock)
    val values :: _ = confMultiplexer multiplex nestedConf

    values must haveSize(6)
  }
}
