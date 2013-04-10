package com.precog.templater

import scala.collection.breakOut
import scala.util.Try
import org.streum.configrity.Configuration
import org.streum.configrity.converter.ValueConverter

/** Breaks the detached configuration for the multiplexer into multiple
  * maps of parameters, each of which has a corresponding path.
  */
class ConfMultiplexer {

  // TODO: Return a Validation
  /** Generate a sequence of parameter maps, each of which gets associated with a path of generation */
  def multiplex(configuration: Configuration): Seq[ConfMap] = {
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

    val commonValues: Map[String, String] = parameters.data.keySet.collect {
      case key if !key.contains('.') => key -> decode(key).toString()
    }(breakOut)

    confType match {
      case Some("list") =>
        val result = for {
          element <- configuration[List[String]]("list")
          subConf <- multiplex(configuration detach element)
        } yield subConf copy (path = element :: subConf.path, parameters = commonValues ++ subConf.parameters)

        result

      case Some("combination") =>
        val combList = for {
          element <- configuration[List[String]]("combination")
        } yield configuration[List[String]](element)

        combList.foldLeft(List(ConfMap(Nil, commonValues))) {
          case (partialResult, list) =>
            for {
              ConfMap(path, parameters) <- partialResult
              element <- list
              subConf <- multiplex(configuration detach element)
            } yield subConf copy (path = path ::: (element :: subConf.path),
                                  parameters = parameters ++ subConf.parameters)
        } map (conf => conf copy (parameters = commonValues ++ conf.parameters))

      // FIXME: unknown types must produce errors
      case Some(_) | None => Seq(ConfMap(path = Nil, parameters = commonValues))
    }
  }
}
