package org.combinators.ep.language.java       /*DD:LD:AD*/

/**
  * Code exists to launch performance analysis of code generation of Java solutions. Not part of the
  * standard code generator framework.
  */
import com.github.javaparser.ast.body.TypeDeclaration
import org.apache.commons.io.FileUtils

import System.nanoTime
import org.combinators.ep.domain.WithDomain
import org.combinators.ep.domain.math.MathDomain
import org.combinators.ep.domain.shape.ShapeDomain
import org.combinators.ep.generator.LanguageIndependentTestGenerator
import org.combinators.ep.language.java._
import org.combinators.ep.language.java.algebra.AlgebraGenerator
import org.combinators.ep.language.java.extensibleVisitor.ExtensibleVisitorGenerator
import org.combinators.ep.language.java.interpreter.InterpreterGenerator
import org.combinators.ep.language.java.oo._
import org.combinators.ep.language.java.trivially.TriviallyGenerator
import org.combinators.ep.language.java.visitor.VisitorGenerator

import scala.collection.JavaConverters._
import java.nio.file.{Files, Paths, StandardOpenOption}

abstract class BaseTest(val id:String) {
  // Each subclass overrides accordingly
  val gen: JavaGenerator with LanguageIndependentTestGenerator

  // time the synthesis of the generated code plus test suites. Output to 'ep'
  def generatedCode(approachName:String, systemName: String): Unit = {
    val now = nanoTime
    val all_code = gen.generatedCode() ++ gen.generateSuite(Some(approachName))
    nanoTime - now
    val outputDir = Paths.get("target", "ep-firstVersion", "java", approachName, systemName)

    println("Cleaning " + outputDir.toAbsolutePath.toString + " ...")
    FileUtils.deleteDirectory(outputDir.toFile)
    Files.createDirectories(outputDir)

    // all code is FLAT in the same directory. Just extract the interface or class name
    all_code.foreach(u => {
      val tpe = u.getTypes.get(0).asTypeDeclaration()
      val pkg = if (tpe.isTopLevelType) {
        if (tpe.findCompilationUnit().get().getPackageDeclaration.isPresent) {
          val pkgName = tpe.findCompilationUnit().get().getPackageDeclaration.get().getNameAsString
          pkgName + "." + tpe.getNameAsString
        } else {
          tpe.getNameAsString
        }
      } else {
        tpe.getNameAsString
      }

      val all = pkg.split("\\.")
      all(all.length-1) = all(all.length-1) + ".java"    // need suffix

      // ALL but the last
      val top = Seq("ep-firstVersion", "java", approachName, systemName) ++ all.init.toSeq
      Files.createDirectories(Paths.get("target", top: _*))

      val the_file = Seq("ep-firstVersion", "java", approachName, systemName) ++ all.toSeq

      //val path = Paths.get("target", "ep-firstVersion", "java", approachName, systemName, clsName)
      val path = Paths.get("target", the_file:_*)
      Files.write(path, u.toString.getBytes, StandardOpenOption.APPEND, StandardOpenOption.CREATE)
    })

    nanoTime
  }
}

  // might be easier way to do this...
object OOEvaluateTest  {
  def name = Some("oo")

  def evaluate(selected:String) : BaseTest = {

    selected match {
      case "e0" => new BaseTest("e0") {
        override val gen = new WithDomain(MathDomain) with OOGenerator with JUnitTestGenerator with e0
      }

      case "e1" => new BaseTest("e1") {
        override val gen = new WithDomain(MathDomain) with OOGenerator with JUnitTestGenerator with e0 with e1
      }

      case "e2" => new BaseTest("e2") {
        override val gen = new WithDomain(MathDomain) with OOGenerator with JUnitTestGenerator with e0 with e1 with e2
      }

      case "e3" => new BaseTest("e3") {
        override val gen = new WithDomain(MathDomain) with OOGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3
      }

      case "e4" => new BaseTest("e4") {
        override val gen = new WithDomain(MathDomain) with OOGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4
      }

      case "e5" => new BaseTest("e5") {
        override val gen = new WithDomain(MathDomain) with OOGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
      }

      case "e6" => new BaseTest("e6") {
        override val gen = new WithDomain(MathDomain) with OOGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6
      }

      case "e7" => new BaseTest("e7") {
        override val gen = new WithDomain(MathDomain) with OOGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6 with e7
      }

      case "e8" => new BaseTest("e8") {
        override val gen = new WithDomain(MathDomain) with OOGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6 with e7 with e8
      }

      case "s0" => new BaseTest("s0") {
        override val gen = new WithDomain(ShapeDomain) with OOGenerator with JUnitTestGenerator with s0
      }

      case "s1" => new BaseTest("s1") {
        override val gen = new WithDomain(ShapeDomain) with OOGenerator with JUnitTestGenerator with s0 with s1
      }

      case _ => ???
    }

  }
}

// might be easier way to do this...
object TriviallyEvaluateTest extends App {
  def name = Some("trivially")

  def evaluate(selected:String) : BaseTest = {
    selected match {
      case "e0" => new BaseTest("e0") {
        override val gen = new WithDomain(MathDomain) with TriviallyGenerator with JUnitTestGenerator with e0
      }

      case "e1" => new BaseTest("e1") {
        override val gen = new WithDomain(MathDomain) with TriviallyGenerator with JUnitTestGenerator with e0 with e1
      }

      case "e2" => new BaseTest("e2") {
        override val gen = new WithDomain(MathDomain) with TriviallyGenerator with JUnitTestGenerator with e0 with e1 with e2
      }

      case "e3" => new BaseTest("e3") {
        override val gen = new WithDomain(MathDomain) with TriviallyGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3
      }

      case "e4" => new BaseTest("e4") {
        override val gen = new WithDomain(MathDomain) with TriviallyGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4
      }

      case "e5" => new BaseTest("e5") {
        override val gen = new WithDomain(MathDomain) with TriviallyGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
      }

      case "e6" => new BaseTest("e6") {
        override val gen = new WithDomain(MathDomain) with TriviallyGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6
      }

      case "e7" => new BaseTest("e7") {
        override val gen = new WithDomain(MathDomain) with TriviallyGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6 with e7
      }

      case "e8" => new BaseTest("e8") {
        override val gen = new WithDomain(MathDomain) with TriviallyGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6 with e7 with e8
      }

      case "s0" => new BaseTest("s0") {
        override val gen = new WithDomain(ShapeDomain) with TriviallyGenerator with JUnitTestGenerator with s0
      }

      case "s1" => new BaseTest("s1") {
        override val gen = new WithDomain(ShapeDomain) with TriviallyGenerator with JUnitTestGenerator with s0 with s1
      }

      case _ => ???
    }
  }
}

// might be easier way to do this...
object AlgebraEvaluateTest extends App {

  def name = Some("algebra")

  def evaluate(selected:String) : BaseTest = {
    selected match {
      case "e0" => new BaseTest("e0") {
        override val gen = new WithDomain(MathDomain) with AlgebraGenerator with JUnitTestGenerator with e0
      }

      case "e1" => new BaseTest("e1") {
        override val gen = new WithDomain(MathDomain) with AlgebraGenerator with JUnitTestGenerator with e0 with e1
      }

      case "e2" => new BaseTest("e2") {
        override val gen = new WithDomain(MathDomain) with AlgebraGenerator with JUnitTestGenerator with e0 with e1 with e2
      }

      case "e3" => new BaseTest("e3") {
        override val gen = new WithDomain(MathDomain) with AlgebraGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3
      }

      case "e4" => new BaseTest("e4") {
        override val gen = new WithDomain(MathDomain) with AlgebraGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4
      }

      case "e5" => new BaseTest("e5") {
        override val gen = new WithDomain(MathDomain) with AlgebraGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
      }

      case "e6" => new BaseTest("e6") {
        override val gen = new WithDomain(MathDomain) with AlgebraGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6
      }

      case "e7" => new BaseTest("e7") {
        override val gen = new WithDomain(MathDomain) with AlgebraGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6 with e7
      }

      case "e8" => new BaseTest("e8") {
        override val gen = new WithDomain(MathDomain) with AlgebraGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6 with e7 with e8
      }

      case "s0" => new BaseTest("s0") {
        override val gen = new WithDomain(ShapeDomain) with AlgebraGenerator with JUnitTestGenerator with s0
      }

      case "s1" => new BaseTest("s1") {
        override val gen = new WithDomain(ShapeDomain) with AlgebraGenerator with JUnitTestGenerator with s0 with s1
      }

      case _ => ???
    }
  }
}

// might be easier way to do this...
object InterpreterEvaluateTest extends App {

  def name = Some("interpreter")

  def evaluate(selected:String) : BaseTest = {
    selected match {
      case "e0" => new BaseTest("e0") {
        override val gen = new WithDomain(MathDomain) with InterpreterGenerator with JUnitTestGenerator with e0
      }

      case "e1" => new BaseTest("e1") {
        override val gen = new WithDomain(MathDomain) with InterpreterGenerator with JUnitTestGenerator with e0 with e1
      }

      case "e2" => new BaseTest("e2") {
        override val gen = new WithDomain(MathDomain) with InterpreterGenerator with JUnitTestGenerator with e0 with e1 with e2
      }

      case "e3" => new BaseTest("e3") {
        override val gen = new WithDomain(MathDomain) with InterpreterGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3
      }

      case "e4" => new BaseTest("e4") {
        override val gen = new WithDomain(MathDomain) with InterpreterGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4
      }

      case "e5" => new BaseTest("e5") {
        override val gen = new WithDomain(MathDomain) with InterpreterGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
      }

      case "e6" => new BaseTest("e6") {
        override val gen = new WithDomain(MathDomain) with InterpreterGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6
      }

      case "e7" => new BaseTest("e7") {
        override val gen = new WithDomain(MathDomain) with InterpreterGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6 with e7
      }

      case "e8" => new BaseTest("e8") {
        override val gen = new WithDomain(MathDomain) with InterpreterGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6 with e7 with e8
      }

      case "s0" => new BaseTest("s0") {
        override val gen = new WithDomain(ShapeDomain) with InterpreterGenerator with JUnitTestGenerator with s0
      }

      case "s1" => new BaseTest("s1") {
        override val gen = new WithDomain(ShapeDomain) with InterpreterGenerator with JUnitTestGenerator with s0 with s1
      }

      case _ => ???
    }
  }
}

// might be easier way to do this...
object VisitorEvaluateTest extends App {
  def name = Some("visitor")

  def evaluate(selected:String) : BaseTest = {
    selected match {
      case "e0" => new BaseTest("e0") {
        override val gen = new WithDomain(MathDomain) with VisitorGenerator with JUnitTestGenerator with e0
      }

      case "e1" => new BaseTest("e1") {
        override val gen = new WithDomain(MathDomain) with VisitorGenerator with JUnitTestGenerator with e0 with e1
      }

      case "e2" => new BaseTest("e2") {
        override val gen = new WithDomain(MathDomain) with VisitorGenerator with JUnitTestGenerator with e0 with e1 with e2
      }

      case "e3" => new BaseTest("e3") {
        override val gen = new WithDomain(MathDomain) with VisitorGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3
      }

      case "e4" => new BaseTest("e4") {
        override val gen = new WithDomain(MathDomain) with VisitorGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4
      }

      case "e5" => new BaseTest("e5") {
        override val gen = new WithDomain(MathDomain) with VisitorGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
      }

      case "e6" => new BaseTest("e6") {
        override val gen = new WithDomain(MathDomain) with VisitorGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6
      }

      case "e7" => new BaseTest("e7") {
        override val gen = new WithDomain(MathDomain) with VisitorGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6 with e7
      }

      case "e8" => new BaseTest("e8") {
        override val gen = new WithDomain(MathDomain) with VisitorGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6 with e7 with e8
      }

      case "s0" => new BaseTest("s0") {
        override val gen = new WithDomain(ShapeDomain) with VisitorGenerator with JUnitTestGenerator with s0
      }

      case "s1" => new BaseTest("s1") {
        override val gen = new WithDomain(ShapeDomain) with VisitorGenerator with JUnitTestGenerator with s0 with s1
      }

      case _ => ???
    }
  }
}

// might be easier way to do this...
object ExtensibleVisitorEvaluateTest extends App {

  def name = Some("extensible")

  def evaluate(selected:String) : BaseTest = {
    selected match {
      case "e0" => new BaseTest("e0") {
        override val gen = new WithDomain(MathDomain) with ExtensibleVisitorGenerator with JUnitTestGenerator with e0
      }

      case "e1" => new BaseTest("e1") {
        override val gen = new WithDomain(MathDomain) with ExtensibleVisitorGenerator with JUnitTestGenerator with e0 with e1
      }

      case "e2" => new BaseTest("e2") {
        override val gen = new WithDomain(MathDomain) with ExtensibleVisitorGenerator with JUnitTestGenerator with e0 with e1 with e2
      }

      case "e3" => new BaseTest("e3") {
        override val gen = new WithDomain(MathDomain) with ExtensibleVisitorGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3
      }

      case "e4" => new BaseTest("e4") {
        override val gen = new WithDomain(MathDomain) with ExtensibleVisitorGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4
      }

      case "e5" => new BaseTest("e5") {
        override val gen = new WithDomain(MathDomain) with ExtensibleVisitorGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
      }

      case "e6" => new BaseTest("e6") {
        override val gen = new WithDomain(MathDomain) with ExtensibleVisitorGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6
      }

      case "e7" => new BaseTest("e7") {
        override val gen = new WithDomain(MathDomain) with ExtensibleVisitorGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6 with e7
      }

      case "e8" => new BaseTest("e8") {
        override val gen = new WithDomain(MathDomain) with ExtensibleVisitorGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6 with e7 with e8
      }

      case "s0" => new BaseTest("s0") {
        override val gen = new WithDomain(ShapeDomain) with ExtensibleVisitorGenerator with JUnitTestGenerator with s0
      }

      case "s1" => new BaseTest("s1") {
        override val gen = new WithDomain(ShapeDomain) with ExtensibleVisitorGenerator with JUnitTestGenerator with s0 with s1
      }

      case _ => ???
    }
  }
}

object GenerateApproach extends App {
  println ("Generating code...")

  // Choose your own adventure
  val approach = if (args.length == 0) {
    "trivially"
  } else {
    args(0)
  }

  val system = if (args.length == 0) {
    "e2"
  } else {
    args(1)
  }

  approach match {
    case "oo" => OOEvaluateTest.evaluate (system).generatedCode (approach, system)
    case "visitor" => VisitorEvaluateTest.evaluate (system).generatedCode (approach, system)
    case "extensibleVisitor" => ExtensibleVisitorEvaluateTest.evaluate (system).generatedCode (approach, system)
    case "interpreter" => InterpreterEvaluateTest.evaluate (system).generatedCode (approach, system)
    case "trivially" => TriviallyEvaluateTest.evaluate (system).generatedCode (approach, system)
    case "algebra" => AlgebraEvaluateTest.evaluate (system).generatedCode (approach, system)
    case _ => ???
  }
}
