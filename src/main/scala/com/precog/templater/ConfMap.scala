package com.precog.templater

/** Map of parameters and the path to them
  */
case class ConfMap(path: List[String], source: Option[String], parameters: Map[String, String])
