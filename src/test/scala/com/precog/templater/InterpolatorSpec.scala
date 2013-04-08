package com.precog.templater

import org.specs2.Specification

/**
  */
class InterpolatorSpec extends Specification {
  def is = s2"""
    An input string without interpolation is returned as is                      $noInterpolation
  """

  def checkConf = {
    failure
  }
}
