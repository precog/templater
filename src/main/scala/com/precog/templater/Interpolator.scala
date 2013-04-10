package com.precog.templater

import scala.tools.reflect.ToolBox
import scala.reflect.runtime.currentMirror

/** Generates interpolated strings based on input strings and parameters.
  */
class Interpolator(parameters: (String, String)*) {
  val startInterpolation = """s""""""
  val endInterpolation = """""""""

  val simpleVals = parameters.collect {
    case (name, value) if !value.contains('$') => s"val $name = $value"
  }

  val codeVals = parameters.collect {
    case (name, value) if value contains '$' => s"val $name = " + startInterpolation + value + endInterpolation
  }

  val prefix = simpleVals ++ codeVals

  val toolBox = currentMirror.mkToolBox()

  /** Interpolate the input string using Scala's s interpolator, with the parameters passed
    * on the constructor in scope.
    */
  def interpolate(template: String): String = {
    val expression = startInterpolation + template + endInterpolation
    val sourceCode = prefix :+ expression mkString "\n"
    val parsedCode = toolBox parse sourceCode
    val result = toolBox eval parsedCode

    result.asInstanceOf[String]
  }

}
