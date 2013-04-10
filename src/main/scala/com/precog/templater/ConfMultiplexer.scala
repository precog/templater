package com.precog.templater

import org.streum.configrity.Configuration
import scala.util.Try
import org.streum.configrity.converter.ValueConverter

/** Breaks the detached configuration for the multiplexer into multiple
  * sets of values.
  */
class ConfMultiplexer {
  def multiplex(configuration: Configuration): Seq[Seq[(String, String)]] = {
    val confType = configuration.get[String]("type")
    val parameters = {
      val typelessConf = configuration clear "type"
      confType map typelessConf.clear getOrElse typelessConf
    }

    def quoteString(string: String) = '"' + string + '"'

    def tryConf[A : ValueConverter](key: String) = Try(parameters[A](key))

    def tryLists(key: String) = (
      tryConf[List[Boolean]](key)
      orElse tryConf[List[Int]](key)
      orElse (tryConf[List[String]](key) map (_ map quoteString))
    ) map (_ mkString ("List(", ", ", ")"))

    def tryAtoms(key: String) = (
      tryConf[Boolean](key)
      orElse tryConf[Int](key)
    )

    def decode(key: String) = tryLists(key) orElse tryAtoms(key) getOrElse quoteString(parameters[String](key))

    val commonValues = parameters.data.keys.toSeq collect {
      case key if !key.contains('.') => key -> decode(key).toString()
    }

    confType match {
      case Some("list") =>
        List.fill(configuration[List[String]]("list").length)(commonValues)

      case Some(_) | None => List(commonValues)
    }
  }
}
