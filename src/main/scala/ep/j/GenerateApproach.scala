package ep.j       /*DD:LD:AD*/

/**
 * Code exists to launch performance analysis of code generation of Java solutions. Not part of the
 * standard code generator framework.
 */
import com.github.javaparser.ast.CompilationUnit
import ep.domain.{MathDomain, ShapeDomain, WithDomain}
import ep.j.algebra.{AlgebraGenerator, AlgebraTestGenerator}
import ep.j.extensibleVisitor.ExtensibleVisitorGenerator
import ep.j.interpreter.InterpreterGenerator
import ep.j.oo.OOGenerator
import ep.j.trivially.TriviallyGenerator
import ep.j.visitor.VisitorGenerator
import org.apache.commons.io.FileUtils

import System.nanoTime
import scala.collection.JavaConverters._
import java.nio.file.{Files, Paths, StandardOpenOption}

abstract class BaseTest(val id:String) {
  // Each subclass overrides accordingly
  val gen: JavaGenerator with JUnitTestGenerator

  var extras: Seq[CompilationUnit] = Seq.empty

  // time the synthesis of the generated code plus test suites. Output to 'ep'
  def generatedCode(approachName:String, systemName: String): Unit = {
    val now = nanoTime
    val all_code = gen.generatedCode() ++ gen.generateSuite(Some(approachName)) ++ extras
    nanoTime - now
    val outputDir = Paths.get("target", "ep-originalPrototype", "java", approachName, systemName)

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
      val top = Seq("ep-originalPrototype", "java", approachName, systemName) ++ all.init.toSeq
      Files.createDirectories(Paths.get("target", top: _*))

      val the_file = Seq("ep-originalPrototype", "java", approachName, systemName) ++ all.toSeq

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

      case "i1" => new BaseTest("i1") {
        override val gen = new WithDomain(MathDomain) with OOGenerator with JUnitTestGenerator with e0 with e1 with i1
      }

      case "i2" => new BaseTest("i2") {
        override val gen = new WithDomain(MathDomain) with OOGenerator with JUnitTestGenerator with e0 with e1 with i1 with i2
      }

      case "c1" => new BaseTest("c1") {
        override val gen = new WithDomain(MathDomain) with OOGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with i1 with i2 with c1
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

      case "i1" => new BaseTest("i1") {
        override val gen = new WithDomain(MathDomain) with TriviallyGenerator with JUnitTestGenerator with e0 with e1 with i1
      }

      case "i2" => new BaseTest("i2") {
        override val gen = new WithDomain(MathDomain) with TriviallyGenerator with JUnitTestGenerator with e0 with e1 with i1 with i2
      }

      case "c1" => new BaseTest("c1") {
        override val gen = new WithDomain(MathDomain) with TriviallyGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with i1 with i2 with c1
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
        override val gen = new WithDomain(MathDomain) with AlgebraGenerator with AlgebraTestGenerator with e0
        extras = extras :+ gen.combinedAlgebra(Some("algebra"), gen.getModel)
      }

      case "e1" => new BaseTest("e1") {
        override val gen = new WithDomain(MathDomain) with AlgebraGenerator with AlgebraTestGenerator with e0 with e1
        extras = extras :+ gen.combinedAlgebra(Some("algebra"), gen.getModel)
      }

      case "e2" => new BaseTest("e2") {
        override val gen = new WithDomain(MathDomain) with AlgebraGenerator with AlgebraTestGenerator with e0 with e1 with e2
        extras = extras :+ gen.combinedAlgebra(Some("algebra"), gen.getModel)
      }

      case "e3" => new BaseTest("e3") {
        override val gen = new WithDomain(MathDomain) with AlgebraGenerator with AlgebraTestGenerator with e0 with e1 with e2 with e3
        extras = extras :+ gen.combinedAlgebra(Some("algebra"), gen.getModel)
      }

      case "e4" => new BaseTest("e4") {
        override val gen = new WithDomain(MathDomain) with AlgebraGenerator with AlgebraTestGenerator with e0 with e1 with e2 with e3 with e4
        extras = extras :+ gen.combinedAlgebra(Some("algebra"), gen.getModel)
      }

      case "e5" => new BaseTest("e5") {
        override val gen = new WithDomain(MathDomain) with AlgebraGenerator with AlgebraTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
        extras = extras :+ gen.combinedAlgebra(Some("algebra"), gen.getModel)
      }

      case "e6" => new BaseTest("e6") {
        override val gen = new WithDomain(MathDomain) with AlgebraGenerator with AlgebraTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6
        extras = extras :+ gen.combinedAlgebra(Some("algebra"), gen.getModel)
      }

      case "i1" => new BaseTest("i1") {
        override val gen = new WithDomain(MathDomain) with AlgebraGenerator with AlgebraTestGenerator with e0 with e1 with i1
        extras = extras :+ gen.combinedAlgebra(Some("algebra"), gen.getModel)
      }

      case "i2" => new BaseTest("i2") {
        override val gen = new WithDomain(MathDomain) with AlgebraGenerator with AlgebraTestGenerator with e0 with e1 with i1 with i2
        extras = extras :+ gen.combinedAlgebra(Some("algebra"), gen.getModel)
      }

      case "c1" => new BaseTest("c1") {
        override val gen = new WithDomain(MathDomain) with AlgebraGenerator with AlgebraTestGenerator with e0 with e1 with e2 with e3 with i1 with i2 with c1
        extras = extras :+ gen.combinedAlgebra(Some("algebra"), gen.getModel)
      }

      case "s0" => new BaseTest("s0") {
        override val gen = new WithDomain(ShapeDomain) with AlgebraGenerator with AlgebraTestGenerator with s0
        extras = extras :+ gen.combinedAlgebra(Some("algebra"), gen.getModel)
      }

      case "s1" => new BaseTest("s1") {
        override val gen = new WithDomain(ShapeDomain) with AlgebraGenerator with AlgebraTestGenerator with s0 with s1
        extras = extras :+ gen.combinedAlgebra(Some("algebra"), gen.getModel)
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

      case "i1" => new BaseTest("i1") {
        override val gen = new WithDomain(MathDomain) with InterpreterGenerator with JUnitTestGenerator with e0 with e1 with i1
      }

      case "i2" => new BaseTest("i2") {
        override val gen = new WithDomain(MathDomain) with InterpreterGenerator with JUnitTestGenerator with e0 with e1 with i1 with i2
      }

      case "c1" => new BaseTest("c1") {
        override val gen = new WithDomain(MathDomain) with InterpreterGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with i1 with i2 with c1
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

      case "i1" => new BaseTest("i1") {
        override val gen = new WithDomain(MathDomain) with VisitorGenerator with JUnitTestGenerator with e0 with e1 with i1
      }

      case "i2" => new BaseTest("i2") {
        override val gen = new WithDomain(MathDomain) with VisitorGenerator with JUnitTestGenerator with e0 with e1 with i1 with i2
      }

      case "c1" => new BaseTest("c1") {
        override val gen = new WithDomain(MathDomain) with VisitorGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with i1 with i2 with c1
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

      case "i1" => new BaseTest("i1") {
        override val gen = new WithDomain(MathDomain) with ExtensibleVisitorGenerator with JUnitTestGenerator with e0 with e1 with i1
      }

      case "i2" => new BaseTest("i2") {
        override val gen = new WithDomain(MathDomain) with ExtensibleVisitorGenerator with JUnitTestGenerator with e0 with e1 with i1 with i2
      }

      case "c1" => new BaseTest("c1") {
        override val gen = new WithDomain(MathDomain) with ExtensibleVisitorGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with i1 with i2 with c1
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
    "extensibleVisitor"
  } else {
    args(0)
  }

  val system = if (args.length == 0) {
    "e3"
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
