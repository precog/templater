package com.precog.templater

import org.specs2.Specification

/**
  */
class InterpolatorSpec extends Specification {

  def is = s2"""
    An input string without interpolation is returned as is                      $noInterpolationTest
    Simple variables are replaced in the interpolated string                     $simpleInterpolationTest
    Scala code is replaced by its evaluation                                     $interpolationWithScalaCodeTest
    Scala code is replaced after evaluating simple variables                     $constantsBeforeCodeTest
  """

  def noInterpolationTest = {
    val interpolator = new Interpolator

    interpolator interpolate "simple string" must beEqualTo("simple string")
  }

  def simpleInterpolationTest = {
    val interpolator = new Interpolator("a" -> "5", "b" -> "\"x\"")

    interpolator interpolate "string with $a and ${b}" must beEqualTo("string with 5 and x")
  }

  def interpolationWithScalaCodeTest = {
    val interpolator = new Interpolator("code" -> "\"${5 * 5}\"")

    interpolator interpolate "evaluating $code" must beEqualTo("evaluating 25")
  }

  def constantsBeforeCodeTest = {
    val interpolator = new Interpolator("code" -> "\"${b * a}\"", "a" -> "5", "b" -> "\"x\"")

    interpolator interpolate "evaluating $code" must beEqualTo("evaluating xxxxx")
  }
}
