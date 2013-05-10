package com.precog.templater

import scala.tools.reflect.ToolBox
import scala.reflect.runtime.currentMirror

/** Generates interpolated strings based on input strings and parameters.
  */
class Interpolator(parameters: (String, String)*) {
  val startInterpolation = """s"""""" // TODO: move constants to an object companion
  val endInterpolation = """""""""

  val simpleVals = parameters.collect {
    case (name, value) if !value.contains('$') => s"val $name = $value"
  }

  def unquote(s: String) = s substring (1, s.length - 1)

  val codeVals = parameters.collect {
    case (name, value) if value contains '$' => s"val $name = " + startInterpolation + unquote(value) + endInterpolation
  }

  val prefix = simpleVals ++ codeVals

  val toolBox = currentMirror.mkToolBox()

  /** Interpolate the input string using Scala's s interpolator, with the parameters passed
    * on the constructor in scope.
    */
  def interpolate(template: String): String = { // FIXME: return Validation
    val expression = startInterpolation + template + endInterpolation
    val sourceCode = prefix :+ expression mkString "\n"
    val parsedCode = toolBox parse sourceCode
    val result = toolBox eval parsedCode // FIXME: catch exceptions and report them as errors

    result.asInstanceOf[String]
  }

}
