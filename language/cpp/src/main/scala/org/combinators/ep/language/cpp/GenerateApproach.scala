package org.combinators.ep.language.cpp    /*DD:LD:AD*/

import org.combinators.ep.domain.WithDomain
import org.combinators.ep.domain.math.MathDomain
import org.combinators.ep.generator.{FileWithPath, LanguageIndependentGenerator, LanguageIndependentTestGenerator}

import java.nio.file.{Files, Paths, StandardOpenOption}
import org.apache.commons.io.FileUtils
import org.combinators.ep.language.cpp.oo.{CPPOOTestGenerator, StraightGenerator}
import org.combinators.ep.language.cpp.visitorTable.{CPPTableTestGenerator, CPPVisitorTableGenerator}
import org.combinators.ep.language.cpp.visitor.{CPPVisitorGenerator, CPPVisitorTestGenerator}

import System.nanoTime

abstract class BaseTest(val id:String) {
  // Each subclass overrides accordingly
  val gen: WithDomain[MathDomain] with LanguageIndependentGenerator with LanguageIndependentTestGenerator

  // time the synthesis of the generated code plus test suites
  def generatedCode(approachName:String, systemName: String): Long = {
    val now = nanoTime
    val all_code = gen.generatedCode() ++ gen.generateSuite(None)
    val cpp_code = all_code.asInstanceOf[Seq[CPPFile]]

    val outputDir = Paths.get("target", "ep-firstVersion", "cpp", approachName, systemName)

    println("Cleaning " + outputDir.toAbsolutePath.toString + " ...")
    FileUtils.deleteDirectory(outputDir.toFile)
    Files.createDirectories(outputDir)

    // all code is FLAT in the same directory. Just extract the interface or class name
    cpp_code.foreach(u => {
      val name = if (u.isHeader) {
        Paths.get(u.fileName + ".h")
      } else {
        Paths.get(u.fileName + ".cpp")
      }
      val path = Paths.get("target", "ep-firstVersion", "cpp", approachName, systemName, name.toString)
      Files.write(path, u.toString.getBytes, StandardOpenOption.APPEND, StandardOpenOption.CREATE)
    })

    nanoTime - now
  }
}


object StraightTest extends App {

  def name = Some("straight")

  def evaluate(selected:String) : BaseTest = {

    selected match {
      case "e0" => new BaseTest("e0") {
        override val gen = new WithDomain(MathDomain) with StraightGenerator with CPPOOTestGenerator with cpp_e0
      }
      case "e1" => new BaseTest("e1") {
        override val gen = new WithDomain(MathDomain) with StraightGenerator with CPPOOTestGenerator with cpp_e0 with cpp_e1
      }
      case "e2" => new BaseTest("e2") {
        override val gen = new WithDomain(MathDomain) with StraightGenerator with CPPOOTestGenerator with cpp_e0 with cpp_e1 with cpp_e2
      }
      case "e3" => new BaseTest("e3") {
        override val gen = new WithDomain(MathDomain) with StraightGenerator with CPPOOTestGenerator with cpp_e0 with cpp_e1 with cpp_e2 with cpp_e3
      }
      case "e4" => new BaseTest("e4") {
        override val gen = new WithDomain(MathDomain) with StraightGenerator with CPPOOTestGenerator with cpp_e0 with cpp_e1 with cpp_e2 with cpp_e3 with cpp_e4
      }

      case _ => ???
    }
  }
}


object VisitorTest extends App {

  def name = Some("visitor")

  def evaluate(selected:String) : BaseTest = {

    selected match {
      case "e0" => new BaseTest("e0") {
        override val gen = new WithDomain(MathDomain) with CPPVisitorGenerator with CPPVisitorTestGenerator with cpp_e0
      }
      case "e1" => new BaseTest("e1") {
        override val gen = new WithDomain(MathDomain) with CPPVisitorGenerator with CPPVisitorTestGenerator with cpp_e0 with cpp_e1
      }
      case "e2" => new BaseTest("e2") {
        override val gen = new WithDomain(MathDomain) with CPPVisitorGenerator with CPPVisitorTestGenerator with cpp_e0 with cpp_e1 with cpp_e2
      }
      case "e3" => new BaseTest("e3") {
        override val gen = new WithDomain(MathDomain) with CPPVisitorGenerator with CPPVisitorTestGenerator with cpp_e0 with cpp_e1 with cpp_e2 with cpp_e3
      }
      case "e4" => new BaseTest("e4") {
        override val gen = new WithDomain(MathDomain) with CPPVisitorGenerator with CPPVisitorTestGenerator with cpp_e0 with cpp_e1 with cpp_e2 with cpp_e3 with cpp_e4
      }

      case _ => ???
    }
  }
}

object VisitorTableTest extends App {

  def name = Some("visitorTable")

  def evaluate(selected:String) : BaseTest = {

    selected match {
      case "e0" => new BaseTest("e0") {
        override val gen = new WithDomain(MathDomain) with CPPVisitorTableGenerator with CPPTableTestGenerator with cpp_e0
      }
      case "e1" => new BaseTest("e1") {
        override val gen = new WithDomain(MathDomain) with CPPVisitorTableGenerator with CPPTableTestGenerator with cpp_e0 with cpp_e1
      }
      case "e2" => new BaseTest("e2") {
        override val gen = new WithDomain(MathDomain) with CPPVisitorTableGenerator with CPPTableTestGenerator with cpp_e0 with cpp_e1 with cpp_e2
      }
      case "e3" => new BaseTest("e3") {
        override val gen = new WithDomain(MathDomain) with CPPVisitorTableGenerator with CPPTableTestGenerator with cpp_e0 with cpp_e1 with cpp_e2 with cpp_e3
      }
      case "e4" => new BaseTest("e4") {
        override val gen = new WithDomain(MathDomain) with CPPVisitorTableGenerator with CPPTableTestGenerator with cpp_e0 with cpp_e1 with cpp_e2 with cpp_e3 with cpp_e4
      }

      case _ => ???
    }
  }
}

object GenerateApproach extends App {
  println ("Generating code...")

  // Choose your own adventure
  val approach = if (args.length == 0) {
    "visitorTable"
  } else {
    args(0)
  }

  // no higher than e4 unfortunately since not yet converting TreeType to target language
  val system = if (args.length == 0) {
    "e0"
  } else {
    args(1)
  }

  approach match {
    case "oo" => StraightTest.evaluate (system).generatedCode (approach, system)
    case "visitor" => VisitorTest.evaluate (system).generatedCode (approach, system)
    case "visitorTable" => VisitorTableTest.evaluate (system).generatedCode (approach, system)

    case _ => ???
  }
}
