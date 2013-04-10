package com.precog.templater

import org.specs2.Specification
import org.streum.configrity.Configuration
import org.streum.configrity.io.BlockFormat

/**
  */
class ConfMultiplexerSpec extends Specification {

  def is = s2"""
    Returns all parameters in a single list for flat configurations  $flatConfigurationIsOneList
    Quotes strings but not booleans and integers                     $quoteStringButNotIntsAndBooleans
    Decodes lists as lists with quoted rules as for scalars          $listsAreListsOfValues
    Does not treat "type" as a parameter                             $typeIsNotParameter
    Does not treat the value of "type" as a parameter                $typeValueIsNotParameter
    Ignores nested blocks as source for parameters                   $ignoreNestedBlocksTest
    Generates lists of configurations for type list                  $oneConfPerElementOfList
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
  val confWithTypeList = conf set ("type", "list") set ("list", List("a", "b", "c"))

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

  def typeIsNotParameter = {
    val values :: _ = confMultiplexer multiplex conf
    val valuesWithType :: _ = confMultiplexer multiplex confWithTypeList

    valuesWithType must beEqualTo(values)
  }

  def typeValueIsNotParameter = {
    val values :: _ = confMultiplexer multiplex conf
    val valuesWithType :: _ = confMultiplexer multiplex confWithTypeList

    valuesWithType must beEqualTo(values)
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

  def oneConfPerElementOfList = {
    val multiplexedValues = confMultiplexer multiplex confWithTypeList

    multiplexedValues must haveSize(3)
  }
}
