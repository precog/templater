package com.precog.templater

import org.streum.configrity.Configuration
import scala.util.Try
import org.streum.configrity.converter.ValueConverter

/** Breaks the detached configuration for the multiplexer into multiple
  * sets of values.
  */
class ConfMultiplexer {
  def multiplex(configuration: Configuration): Seq[Seq[(String, String)]] = {
    def quoteString(string: String) = '"' + string + '"'

    def tryConf[A : ValueConverter](key: String) = Try(configuration[A](key))

    def tryLists(key: String) = (
      tryConf[List[Boolean]](key)
      orElse tryConf[List[Int]](key)
      orElse (tryConf[List[String]](key) map (_ map quoteString))
    ) map (_ mkString ("List(", ", ", ")"))

    def tryAtoms(key: String) = (
      tryConf[Boolean](key)
      orElse tryConf[Int](key)
    )

    def decode(key: String) = tryLists(key) orElse tryAtoms(key) getOrElse quoteString(configuration[String](key))

    val commonValues = configuration.data.keys.toSeq collect {
      case key if !key.contains('.') => key -> decode(key).toString()
    }

    List(commonValues)
  }

}
