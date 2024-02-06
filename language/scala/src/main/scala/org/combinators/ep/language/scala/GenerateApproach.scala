package org.combinators.ep.language.scala


import org.combinators.ep.domain.WithDomain
import org.combinators.ep.domain.math.MathDomain

import java.nio.file.{Files, Paths, StandardOpenOption}
import org.apache.commons.io.FileUtils
import org.combinators.ep.generator.{LanguageIndependentGenerator, LanguageIndependentTestGenerator}
import org.combinators.ep.language.scala.functional.{FunSpecFunctionalTestGenerator, FunctionalGenerator}
import org.combinators.ep.language.scala.oo.OderskyGenerator
import org.combinators.ep.language.scala.straight.OOGenerator

import System.nanoTime

abstract class BaseTest(val id:String) {
  // Each subclass overrides accordingly
  val gen: WithDomain[MathDomain] with LanguageIndependentGenerator with LanguageIndependentTestGenerator

  // time the synthesis of the generated code plus test suites
  def generatedCode(approachName:String, systemName: String): Long = {
    val now = nanoTime
    val all_code = gen.generatedCode() ++ gen.generateSuite(None)
    val scala_code = all_code.asInstanceOf[Seq[ScalaWithPath]]

    val outputDir = Paths.get("target", "ep-firstVersion", "scala", approachName, systemName)

    println("Cleaning " + outputDir.toAbsolutePath.toString + " ...")
    FileUtils.deleteDirectory(outputDir.toFile)
    Files.createDirectories(outputDir)

    // all code is FLAT in the same directory. Just extract the interface or class name
    scala_code.foreach(u => {
      val path = Paths.get("target", "ep-firstVersion",  "scala", approachName, systemName, u.persistTo.toString)
      Files.write(path, u.code.toString.getBytes, StandardOpenOption.APPEND, StandardOpenOption.CREATE)
    })

    nanoTime - now
  }
}

object OOTest extends App {

  def name = Some("oo")

  def evaluate(selected:String) : BaseTest = {

    selected match {
      case "e0" => new BaseTest("e0") {
        override val gen = new WithDomain(MathDomain) with OderskyGenerator with FunSpecTestGenerator with e0
      }
      case "e1" => new BaseTest("e1") {
        override val gen = new WithDomain(MathDomain) with OderskyGenerator with FunSpecTestGenerator with e0 with e1
      }
      case "e2" => new BaseTest("e2") {
        override val gen = new WithDomain(MathDomain) with OderskyGenerator with FunSpecTestGenerator with e0 with e1 with e2
      }
      case "e3" => new BaseTest("e3") {
        override val gen = new WithDomain(MathDomain) with OderskyGenerator with FunSpecTestGenerator with e0 with e1 with e2 with e3
      }
      case "e4" => new BaseTest("e4") {
        override val gen = new WithDomain(MathDomain) with OderskyGenerator with FunSpecTestGenerator with e0 with e1 with e2 with e3 with e4
      }

      case _ => ???
    }
  }
}


object StraightTest extends App {

  def name = Some("straight")

  def evaluate(selected:String) : BaseTest = {

    selected match {
      case "e0" => new BaseTest("e0") {
        override val gen = new WithDomain(MathDomain) with OOGenerator with FunSpecTestGenerator with e0
      }
      case "e1" => new BaseTest("e1") {
        override val gen = new WithDomain(MathDomain) with OOGenerator with FunSpecTestGenerator with e0 with e1
      }
      case "e2" => new BaseTest("e2") {
        override val gen = new WithDomain(MathDomain) with OOGenerator with FunSpecTestGenerator with e0 with e1 with e2
      }
      case "e3" => new BaseTest("e3") {
        override val gen = new WithDomain(MathDomain) with OOGenerator with FunSpecTestGenerator with e0 with e1 with e2 with e3
      }
      case "e4" => new BaseTest("e4") {
        override val gen = new WithDomain(MathDomain) with OOGenerator with FunSpecTestGenerator with e0 with e1 with e2 with e3 with e4
      }


      case _ => ???
    }
  }
}

object FunctionalTest extends App {

  def name = Some("functional")

  def evaluate(selected:String) : BaseTest = {

    selected match {
      case "e0" => new BaseTest("e0") {
        override val gen = new WithDomain(MathDomain) with FunctionalGenerator with FunSpecFunctionalTestGenerator with e0
      }
      case "e1" => new BaseTest("e1") {
        override val gen = new WithDomain(MathDomain) with FunctionalGenerator with FunSpecFunctionalTestGenerator with e0 with e1
      }
      case "e2" => new BaseTest("e2") {
        override val gen = new WithDomain(MathDomain) with FunctionalGenerator with FunSpecFunctionalTestGenerator with e0 with e1 with e2
      }
      case "e3" => new BaseTest("e3") {
        override val gen = new WithDomain(MathDomain) with FunctionalGenerator with FunSpecFunctionalTestGenerator with e0 with e1 with e2 with e3
      }
      case "e4" => new BaseTest("e4") {
        override val gen = new WithDomain(MathDomain) with FunctionalGenerator with FunSpecFunctionalTestGenerator with e0 with e1 with e2 with e3 with e4
      }


      case _ => ???
    }
  }
}

object GenerateApproach extends App {
  println ("Generating code...")

  // Choose your own adventure. Cannot go higher than e4 for now...
  val approach = "oo"
  val system = "e4"

  approach match {
    case "straight" => StraightTest.evaluate (system).generatedCode (approach, system)
    case "oo" => OOTest.evaluate (system).generatedCode (approach, system)
    case "functional" => FunctionalTest.evaluate (system).generatedCode (approach, system)

    case _ => ???
  }
}
