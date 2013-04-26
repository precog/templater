package com.precog.templater

import org.specs2.Specification
import org.streum.configrity.Configuration
import org.streum.configrity.io.BlockFormat

/**
  */
class ConfMultiplexerSpec extends Specification {

  def is = s2"""
    Multiplexing parameters
      Returns all parameters in a single map for flat configurations        $flatConfigurationIsListOfOneMap
      Quotes strings but not booleans and integers                          $quoteStringButNotIntsAndBooleans
      Decodes lists as lists with quoted rules as for scalars               $listsAreListsOfValues

    Special parameters
      Does not treat "type" or "templateSource" as a parameters             $ignoresSpecialParameters
      Does not treat the value of "type" as a parameter                     $typeValueIsNotParameter
      Ignores nested blocks as source for parameters                        $ignoreNestedBlocksTest
      Returns the value of templateSource if found                          $returnsTemplateSource

    Multiplexing lists
      Generates lists of configurations for type list                       $oneConfPerElementOfList
      Includes parameters from nested configurations listed by list         $listsReturnsParameters
      Does not include parameters from nested configurations in all lists   $oneListPerNestedConf
      Handles nested lists                                                  $listsCanBeNested
      Overrides parameters instead of duplicating them                      $parametersAreOverridden
      Prefer the values of nested parameters when overriding                $nestedParametersArePreferred
      Returns paths corresponding to each parameter set                     $nestedConfsReturnPaths
      Give precedence to nested templateSource                              $nestedTemplateSourceArePreferred

    Multiplexing combinations
      Generates lists of configurations for every combination               $combinationsAreCartesianProducts
      Includes parameters from each combined configuration                  $combinationParametersAreCombined
      Does not include parameters from other combinations in each           $combinationsAreNotMixed
      Prefer values of nested parameters when overriding                    $combinationParametersArePreferred
      Overrides parameters from early sets by late sets                     $combinationsOverrideLeftToRight
      Give precedence to nested templateSource                              $combinationsPreferNestedTemplateSource
  """

  val confMultiplexer = new ConfMultiplexer

  val sourceConfiguration =
    """
      |templater {
      |  x = 5
      |  y = true
      |  z = abc
      |  xx = [1, 2, 3]
      |  yy = [true, false]
      |  zz = [abc, def]
      |}
    """.stripMargin

  val conf = Configuration parse (sourceConfiguration, BlockFormat) detach "templater"

  def flatConfigurationIsListOfOneMap = {
    // FIXME: make this test work with non-List returns
    val multiplexedValues @ (ConfMap(_, _, values) :: _) = confMultiplexer multiplex conf

    (multiplexedValues must haveSize(1)) and
      (values must haveSize(6))
  }

  def quoteStringButNotIntsAndBooleans = {
    // FIXME: make this test work with non-List returns
    val ConfMap(_, _, values) :: _ = confMultiplexer multiplex conf

    values must haveAllElementsLike {
      case ("x", xValue) => xValue === "5"
      case ("y", yValue) => yValue === "true"
      case ("z", zValue) => zValue === "\"abc\""
    }
  }

  val confWithTypeList = {
    val aBlock = Configuration("aParam" -> 1)
    val bBlock = Configuration("bParam" -> true)
    val cBlock = Configuration("cParam" -> "c")

    (conf
      set ("type", "list")
      set ("list", List("a", "b", "c"))
      set ("templateSource", "a/b/c")
      attach ("a", aBlock)
      attach ("b", bBlock)
      attach ("c", cBlock)
      )
  }

  def listsAreListsOfValues = {
    // FIXME: make this test work with non-List returns
    val ConfMap(_, _, values) :: _ = confMultiplexer multiplex conf

    values must haveAllElementsLike {
      case ("xx", xValue) => xValue === "List(1, 2, 3)"
      case ("yy", yValue) => yValue === "List(true, false)"
      case ("zz", zValue) => zValue === """List("abc", "def")"""
    }
  }

  def ignoresSpecialParameters = {
    val Seq(ConfMap(_, _, valuesWithType), _*) = confMultiplexer multiplex confWithTypeList

    (valuesWithType must not haveKey("type")) and
      (valuesWithType must not haveKey("templateSource"))
  }

  def typeValueIsNotParameter = {
    val Seq(ConfMap(_, _, valuesWithType), _*) = confMultiplexer multiplex confWithTypeList

    valuesWithType must not haveKey("list")
  }

  def ignoreNestedBlocksTest = {
    val nestedBlock =
      """
        |  a = 5
        |  b = 3
      """.stripMargin

    val nestedConf = conf attach ("subBlock", Configuration parse nestedBlock)
    val Seq(ConfMap(_, _, values), _*) = confMultiplexer multiplex nestedConf

    values must haveSize(6)
  }

  def returnsTemplateSource = {
    val Seq(ConfMap(_, source, _), _*) = confMultiplexer multiplex confWithTypeList

    source must beSome("a/b/c")
  }

  def oneConfPerElementOfList = {
    val multiplexedValues = confMultiplexer multiplex confWithTypeList

    multiplexedValues must haveSize(3)
  }

  def listsReturnsParameters = {
    // FIXME: make this test not order-dependent
    // FIXME: make this test work with non-List returns
    val ConfMap(_, _, valuesA) :: ConfMap(_, _, valuesB) :: ConfMap(_, _, valuesC) :: _ = confMultiplexer multiplex confWithTypeList

    (valuesA must contain ("aParam" -> "1")) and
      (valuesB must contain ("bParam" -> "true")) and
      (valuesC must contain ("cParam" -> "\"c\""))
  }

  def oneListPerNestedConf = {
    // FIXME: make this test not order-dependent
    // FIXME: make this test work with non-List returns
    val ConfMap(_, _, valuesA) :: ConfMap(_, _, valuesB) :: ConfMap(_, _, valuesC) :: _ = confMultiplexer multiplex confWithTypeList

    (valuesB must not haveKey ("aParam")) and
      (valuesC must not haveKey ("aParam")) and
      (valuesA must not haveKey ("bParam")) and
      (valuesC must not haveKey ("bParam")) and
      (valuesA must not haveKey ("cParam")) and
      (valuesB must not haveKey ("cParam"))
  }

  val deeplyNestedListConf = {
    val bBlock = Configuration("b" -> true, "x" -> 6, "templateSource" -> "y")
    val aBlock = Configuration("type" -> "list", "list" -> List("b")) attach ("b", bBlock)

    conf set ("type", "list") set ("list", List("a")) set ("templateSource", "x") attach ("a", aBlock)
  }

  def listsCanBeNested = {
    // FIXME: make this test work with non-List returns
    val multiplexedValues @ (ConfMap(_, _, valuesA) :: _) = confMultiplexer multiplex deeplyNestedListConf

    (multiplexedValues must haveSize(1)) and
      (valuesA must haveOneElementLike { case ("b", bValue) => bValue === "true"})
  }

  def parametersAreOverridden = {
    // FIXME: make this test work with non-List returns
    val ConfMap(_, _, values) :: _ = confMultiplexer multiplex conf
    val ConfMap(_, _, valuesA) :: _ = confMultiplexer multiplex deeplyNestedListConf

    valuesA must haveSize(values.size + 1) // deeplyNestedListConf adds a parameter and overrides another
  }

  def nestedParametersArePreferred = {
    val Seq(ConfMap(_, _, valuesA), _*) = confMultiplexer multiplex deeplyNestedListConf

    valuesA must haveOneElementLike {
      case ("x", xValue) => xValue === "6"
    }
  }

  def nestedConfsReturnPaths = {
    val Seq(ConfMap(path, _, _), _*) = confMultiplexer multiplex deeplyNestedListConf

    path must beEqualTo(List("a", "b"))
  }

  def nestedTemplateSourceArePreferred = {
    val Seq(ConfMap(_, source, _), _*) = confMultiplexer multiplex deeplyNestedListConf

    source must beSome("y")
  }

  val confWithTypeCombination = {
    val a1Block = Configuration("aParam" -> 1, "a1" -> "x", "x" -> 6)
    val a2Block = Configuration("aParam" -> 2, "a2" -> "y", "x" -> 6)
    val a3Block = Configuration("aParam" -> 3, "a3" -> "z")
    val b1Block = Configuration("bParam" -> true, "b1" -> "w", "templateSource" -> "y")
    val b2Block = Configuration("bParam" -> false, "b2" -> "ww", "x" -> 7)

    (conf
      set ("type", "combination")
      set ("combination", List("a", "b"))
      set ("a", List("a1", "a2", "a3"))
      set ("b", List("b1", "b2"))
      set ("templateSource", "x")
      attach ("a1", a1Block)
      attach ("a2", a2Block)
      attach ("a3", a3Block)
      attach ("b1", b1Block)
      attach ("b2", b2Block)
      )
  }

  def combinationsAreCartesianProducts = {
    val multiplexedValues = confMultiplexer multiplex confWithTypeCombination

    multiplexedValues must haveSize(6)
  }

  def combinationParametersAreCombined = {
    val multiplexedValues = confMultiplexer multiplex confWithTypeCombination

    multiplexedValues must haveAllElementsLike {
      case ConfMap(List("a1", "b1"), _, valuesA1B1) =>
        valuesA1B1 must haveAllElementsLike {
          case ("aParam", aValue) => aValue === "1"
          case ("bParam", bValue) => bValue === "true"
        }
      case ConfMap(List("a2", "b1"), _, valuesA2B1) =>
        valuesA2B1 must haveAllElementsLike {
          case ("aParam", aValue) => aValue === "2"
          case ("bParam", bValue) => bValue === "true"
        }
      case ConfMap(List("a3", "b1"), _, valuesA3B1) =>
        valuesA3B1 must haveAllElementsLike {
          case ("aParam", aValue) => aValue === "3"
          case ("bParam", bValue) => bValue === "true"
        }
      case ConfMap(List("a1", "b2"), _, valuesA1B2) =>
        valuesA1B2 must haveAllElementsLike {
          case ("aParam", aValue) => aValue === "1"
          case ("bParam", bValue) => bValue === "false"
        }
      case ConfMap(List("a2", "b2"), _, valuesA2B2) =>
        valuesA2B2 must haveAllElementsLike {
          case ("aParam", aValue) => aValue === "2"
          case ("bParam", bValue) => bValue === "false"
        }
      case ConfMap(List("a3", "b2"), _, valuesA3B2) =>
        valuesA3B2 must haveAllElementsLike {
          case ("aParam", aValue) => aValue === "3"
          case ("bParam", bValue) => bValue === "false"
        }
    }
  }

    def combinationsAreNotMixed = {
      val multiplexedValues = confMultiplexer multiplex confWithTypeCombination

      multiplexedValues must haveAllElementsLike {
        case ConfMap(List("a1", "b1"), _, valuesA1B1) =>
          valuesA1B1 must not haveKeys("a2", "a3", "b2")
        case ConfMap(List("a2", "b1"), _, valuesA2B1) =>
          valuesA2B1 must not haveKeys("a1", "a3", "b2")
        case ConfMap(List("a3", "b1"), _, valuesA3B1) =>
          valuesA3B1 must not haveKeys("a1", "a2", "b2")
        case ConfMap(List("a1", "b2"), _, valuesA1B2) =>
          valuesA1B2 must not haveKeys("a2", "a3", "b1")
        case ConfMap(List("a2", "b2"), _, valuesA2B2) =>
          valuesA2B2 must not haveKeys("a1", "a3", "b1")
        case ConfMap(List("a3", "b2"), _, valuesA3B2) =>
          valuesA3B2 must not haveKeys("a1", "a2", "b1")
      }
  }

  def combinationParametersArePreferred = {
    val multiplexedValues = confMultiplexer multiplex confWithTypeCombination

    multiplexedValues must haveAllElementsLike {
      case ConfMap(List("a1", "b1"), _, valuesA1B1) =>
        valuesA1B1 must havePair("x", "6")
    }
  }

  def combinationsOverrideLeftToRight = {
    val multiplexedValues = confMultiplexer multiplex confWithTypeCombination

    multiplexedValues must haveAllElementsLike {
      case ConfMap(List("a2", "b2"), _, valuesA1B1) =>
        valuesA1B1 must havePair("x", "7")
    }
  }

  def combinationsPreferNestedTemplateSource = {
    val multiplexedValues = confMultiplexer multiplex confWithTypeCombination

    multiplexedValues must haveAllElementsLike {
      case ConfMap(List("a1", "b1"), source, _) =>
        source must beSome("y")
    }
  }

  """
    |templater {
    |  type = list
    |  list = [standard, shardsOnly]
    |
    |  standard {
    |    type = combination
    |    combination = [ services, environments, instances ]
    |    services = [ shard, ingest, auth, accounts, jobs ]
    |    environments = [ dev, beta, special ]
    |    instances = [ main, b ]
    |
    |    templateSource = commonFiles
    |
    |    shard {
    |      servicePrefix = shard
    |    }
    |    main {
    |      serviceSuffix = ""
    |    }
    |    b {
    |      serviceSuffix = "-b"
    |    }
    |    beta {
    |      serviceEnv = "-beta"
    |    }
    |
    |    serviceEnv = ""
    |    serviceName = "$servicePrefix$serviceEnv$serviceSuffix"
    |  }
    |}
    |
  """.stripMargin
}

