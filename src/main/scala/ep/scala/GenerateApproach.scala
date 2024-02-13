package ep.scala     /*DD:LD:AD*/


import ep.domain.{MathDomain, WithDomain}
import ep.generator.LanguageIndependentGenerator
import ep.scala.functional.{FunSpecFunctionalTestGenerator, FunctionalGenerator}
import ep.scala.oo.{FunSpecOOTestGenerator, OderskyGenerator}
import ep.scala.straight.OOGenerator

import java.nio.file.{Files, Paths, StandardOpenOption}
import org.apache.commons.io.FileUtils

import System.nanoTime

abstract class BaseTest(val id:String) {
  // Each subclass overrides accordingly
  val gen: WithDomain[MathDomain] with LanguageIndependentGenerator with FunSpecTestGenerator

  // time the synthesis of the generated code plus test suites
  def generatedCode(approachName:String, systemName: String, pkg: Option[String]): Long = {
    val now = nanoTime
    val all_code = gen.generatedCode() ++ gen.generateSuite(pkg)
    val scala_code = all_code.asInstanceOf[Seq[ScalaWithPath]]

    val outputDir = Paths.get("target", "ep-originalPrototype", "scala", approachName, systemName)

    println("Cleaning " + outputDir.toAbsolutePath.toString + " ...")
    FileUtils.deleteDirectory(outputDir.toFile)
    Files.createDirectories(outputDir)

    // all code is FLAT in the same directory. Just extract the interface or class name
    scala_code.foreach(u => {
      val path = Paths.get("target", "ep-originalPrototype",  "scala", approachName, systemName, u.persistTo.toString)
      Files.createDirectories(path.getParent)   // just in case...
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
        override val gen = new WithDomain(MathDomain) with OderskyGenerator with FunSpecOOTestGenerator with e0
      }
      case "e1" => new BaseTest("e1") {
        override val gen = new WithDomain(MathDomain) with OderskyGenerator with FunSpecOOTestGenerator with e0 with e1
      }
      case "e2" => new BaseTest("e2") {
        override val gen = new WithDomain(MathDomain) with OderskyGenerator with FunSpecOOTestGenerator with e0 with e1 with e2
      }
      case "e3" => new BaseTest("e3") {
        override val gen = new WithDomain(MathDomain) with OderskyGenerator with FunSpecOOTestGenerator with e0 with e1 with e2 with e3
      }
      case "e4" => new BaseTest("e4") {
        override val gen = new WithDomain(MathDomain) with OderskyGenerator with FunSpecOOTestGenerator with e0 with e1 with e2 with e3 with e4
      }
      case "e5" => new BaseTest("e5") {
        override val gen = new WithDomain(MathDomain) with OderskyGenerator with FunSpecOOTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
      }
      case "e6" => new BaseTest("e6") {
        override val gen = new WithDomain(MathDomain) with OderskyGenerator with FunSpecOOTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6
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
      case "e5" => new BaseTest("e5") {
        override val gen = new WithDomain(MathDomain) with OOGenerator with FunSpecTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
      }
      case "e6" => new BaseTest("e6") {
        override val gen = new WithDomain(MathDomain) with OOGenerator with FunSpecTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6
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
      case "e5" => new BaseTest("e5") {
        override val gen = new WithDomain(MathDomain) with FunctionalGenerator with FunSpecFunctionalTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
      }
      case "e6" => new BaseTest("e6") {
        override val gen = new WithDomain(MathDomain) with FunctionalGenerator with FunSpecFunctionalTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6
      }

      case _ => ???
    }
  }
}

object GenerateApproach extends App {
  println ("Generating code...")

  // Choose your own adventure. Cannot go higher than e4 for now...
  val approach = "straight"
  val system = "e0"

  approach match {
    case "straight" => StraightTest.evaluate (system).generatedCode (approach, system, Some("scala_oo"))
    case "oo" => OOTest.evaluate (system).generatedCode (approach, system, Some("odersky"))
    case "functional" => FunctionalTest.evaluate (system).generatedCode (approach, system, Some("scala_func"))

    case _ => ???
  }
}
