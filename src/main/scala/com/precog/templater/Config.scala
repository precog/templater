package com.precog.templater

/** Configuration for templater, to be used with scopt
  */
case class Config(sources: List[String] = Nil, targetDir: String = ".", configPath: String = "templater.conf")
