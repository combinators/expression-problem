package ep.gj   /*DD:LD:AD*/

/**
 * Code exists to launch performance analysis of code generation of Java solutions. Not part of the
 * standard code generator framework.
 */
import ep.domain.{MathDomain, WithDomain}
import ep.generator.LanguageIndependentGenerator

import System.nanoTime
import java.nio.file.{Files, Paths, StandardOpenOption}
import org.apache.commons.io.FileUtils

abstract class BaseTest(val id:String) {
  // Each subclass overrides accordingly
  val gen: WithDomain[MathDomain] with LanguageIndependentGenerator with TestGenerator

  // time the synthesis of the generated code plus test suites
  def generatedCode(approachName:String, systemName: String): Long = {
    val now = nanoTime
    val all_code = gen.generatedCode() ++ gen.generateSuite(None)
    val gj_code = all_code.asInstanceOf[Seq[GJWithPath]]

    val outputDir = Paths.get("target", "ep-originalPrototype", "gj", approachName, systemName)

    println("Cleaning " + outputDir.toAbsolutePath.toString + " ...")
    FileUtils.deleteDirectory(outputDir.toFile)
    Files.createDirectories(outputDir)

    // all code is FLAT in the same directory. Just extract the interface or class name
    gj_code.foreach(u => {
      val path = Paths.get("target", "ep-originalPrototype", "gj", approachName, systemName, u.persistTo.toString)
      Files.write(path, u.code.getCode.getBytes, StandardOpenOption.APPEND, StandardOpenOption.CREATE)
    })

    nanoTime - now
  }
}

object WadlerTest extends App {

  def name = Some("wadler")

  def evaluate(selected:String) : BaseTest = {

    selected match {
      case "e0" => new BaseTest("e0") {
        override val gen = new WithDomain(MathDomain) with WadlerGenerator with TestGenerator with e0
      }
      case "e1" => new BaseTest("e1") {
        override val gen = new WithDomain(MathDomain) with WadlerGenerator with TestGenerator with e0 with e1
      }

      case _ => ???
    }
  }
}

object GenerateApproach extends App {
  println ("Generating code...")

  // Choose your own adventure
  val approach = "wadler"
  val system = "e1"

  approach match {
    case "wadler" => WadlerTest.evaluate (system).generatedCode (approach, system)

    case _ => ???
  }
}
