package org.combinators.ep.language.haskell

/**
  * Code exists to launch performance analysis of code generation of Java solutions. Not part of the
  * standard code generator framework.
  */
import System.nanoTime

import org.combinators.ep.domain.WithDomain
import org.combinators.ep.domain.math.MathDomain
import org.combinators.ep.generator.{LanguageIndependentGenerator, LanguageIndependentTestGenerator}
import org.combinators.ep.language.haskell.alacarte.{ALaCarteGenerator, ALaCarteTestGenerator}
import org.combinators.ep.language.haskell._
import org.combinators.ep.language.haskell.grow.{GrowGenerator, GrowTestGenerator}
import org.combinators.ep.language.haskell.straight.{StraightGenerator, StraightTestGenerator}

import java.nio.file.{Files, Paths, StandardOpenOption}
import org.apache.commons.io.FileUtils

abstract class BaseTest(val id:String) {
  // Each subclass overrides accordingly
  val gen: WithDomain[MathDomain] with LanguageIndependentGenerator with LanguageIndependentTestGenerator

  // time the synthesis of the generated code plus test suites
  def generatedCode(approachName:String, systemName: String): Long = {
    val now = nanoTime
    val all_code = gen.generatedCode() ++ gen.generateSuite(None)
    val haskell_code = all_code.asInstanceOf[Seq[HaskellWithPath]]

    val outputDir = Paths.get("target", "ep-firstVersion", "haskell", approachName, systemName)

    println("Cleaning " + outputDir.toAbsolutePath.toString + " ...")
    FileUtils.deleteDirectory(outputDir.toFile)
    Files.createDirectories(outputDir)

    // all code is FLAT in the same directory. Just extract the interface or class name
    haskell_code.foreach(u => {
      val path = Paths.get("target", "ep-firstVersion", "haskell", approachName, systemName, u.persistTo.toString)
      Files.write(path, u.code.getCode.getBytes, StandardOpenOption.APPEND, StandardOpenOption.CREATE)
    })

    nanoTime - now
  }
}

object GrowTest extends App {

  def name = Some("grow")

  def evaluate(selected:String) : BaseTest = {

    selected match {
      case "e0" => new BaseTest("e0") {
        override val gen = new WithDomain(MathDomain) with GrowGenerator with GrowTestGenerator with e0
      }
      case "e1" => new BaseTest("e1") {
        override val gen = new WithDomain(MathDomain) with GrowGenerator with GrowTestGenerator with e0 with e1
      }
      case "e2" => new BaseTest("e2") {
        override val gen = new WithDomain(MathDomain) with GrowGenerator with GrowTestGenerator with e0 with e1 with e2
      }
      case "e3" => new BaseTest("e3") {
        override val gen = new WithDomain(MathDomain) with GrowGenerator with GrowTestGenerator with e0 with e1 with e2 with e3
      }
      case "e4" => new BaseTest("e4") {
        override val gen = new WithDomain(MathDomain) with GrowGenerator with GrowTestGenerator with e0 with e1 with e2 with e3 with e4
      }
      case "e5" => new BaseTest("e5") {
        override val gen = new WithDomain(MathDomain) with GrowGenerator with GrowTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
      }
      case "e6" => new BaseTest("e6") {
        override val gen = new WithDomain(MathDomain) with GrowGenerator with GrowTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6
      }

      case _ => ???
    }
  }
}

object ALaCarteTest extends App {

  def name = Some("alacarte")

  def evaluate(selected:String) : BaseTest = {

    selected match {
      case "e0" => new BaseTest("e0") {
        override val gen = new WithDomain(MathDomain) with ALaCarteGenerator with ALaCarteTestGenerator with e0
      }
      case "e1" => new BaseTest("e1") {
        override val gen = new WithDomain(MathDomain) with ALaCarteGenerator with ALaCarteTestGenerator with e0 with e1
      }
      case "e2" => new BaseTest("e2") {
        override val gen = new WithDomain(MathDomain) with ALaCarteGenerator with ALaCarteTestGenerator with e0 with e1 with e2
      }
      case "e3" => new BaseTest("e3") {
        override val gen = new WithDomain(MathDomain) with ALaCarteGenerator with ALaCarteTestGenerator with e0 with e1 with e2 with e3
      }
      case "e4" => new BaseTest("e4") {
        override val gen = new WithDomain(MathDomain) with ALaCarteGenerator with ALaCarteTestGenerator with e0 with e1 with e2 with e3 with e4
      }
      case "e5" => new BaseTest("e5") {
        override val gen = new WithDomain(MathDomain) with ALaCarteGenerator with ALaCarteTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
      }
      case "e6" => new BaseTest("e6") {
        override val gen = new WithDomain(MathDomain) with ALaCarteGenerator with ALaCarteTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6
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
        override val gen = new WithDomain(MathDomain) with StraightGenerator with StraightTestGenerator with e0
      }
      case "e1" => new BaseTest("e1") {
        override val gen = new WithDomain(MathDomain) with StraightGenerator with StraightTestGenerator with e0 with e1
      }
      case "e2" => new BaseTest("e2") {
        override val gen = new WithDomain(MathDomain) with StraightGenerator with StraightTestGenerator with e0 with e1 with e2
      }
      case "e3" => new BaseTest("e3") {
        override val gen = new WithDomain(MathDomain) with StraightGenerator with StraightTestGenerator with e0 with e1 with e2 with e3
      }
      case "e4" => new BaseTest("e4") {
        override val gen = new WithDomain(MathDomain) with StraightGenerator with StraightTestGenerator with e0 with e1 with e2 with e3 with e4
      }
      case "e5" => new BaseTest("e5") {
        override val gen = new WithDomain(MathDomain) with StraightGenerator with StraightTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
      }
      case "e6" => new BaseTest("e6") {
        override val gen = new WithDomain(MathDomain) with StraightGenerator with StraightTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6
      }

      case _ => ???
    }
  }
}

object GenerateApproach extends App {
  println ("Generating code...")

  // Choose your own adventure
  val approach = "grow"
  val system = "e1"

  approach match {
    case "straight" => StraightTest.evaluate (system).generatedCode (approach, system)
    case "alacarte" => ALaCarteTest.evaluate (system).generatedCode (approach, system)
    case "grow" => GrowTest.evaluate (system).generatedCode (approach, system)

    case _ => ???
  }
}