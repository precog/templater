package com.precog.templater

/** Configuration for templater, to be used with scopt
  */
case class Config(source: String = "", targetDir: String = ".", configPath: String = "templater.conf")
