package com.precog.templater

import scala.tools.reflect.ToolBox
import scala.reflect.runtime.currentMirror

/** Generates interpolated strings based on input strings and parameters.
  */
class Interpolator(vals: (String, String)*) {
  val startInterpolation = """s""""""
  val endInterpolation = """""""""

  val simpleVals = vals.collect {
    case (name, value) if !value.contains('$') => s"val $name = $value"
  }

  val codeVals = vals.collect {
    case (name, value) if value contains '$' => s"val $name = " + startInterpolation + value + endInterpolation
  }

  val prefix = simpleVals ++ codeVals

  val toolBox = currentMirror.mkToolBox()

  def interpolate(template: String): String = {
    val expression = startInterpolation + template + endInterpolation
    val sourceCode = prefix :+ expression mkString "\n"
    val parsedCode = toolBox parse sourceCode
    val result = toolBox eval parsedCode

    result.asInstanceOf[String]
  }

}
