package example.expression.extensibleVisitor    /*DI:LD:AD*/

import com.github.javaparser.ast.body.TypeDeclaration
import example.expression.domain.{BaseDomain, ModelDomain}
import example.expression.j.{AbstractGenerator, TestGenerator}
import org.combinators.templating.twirl.Java

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait ExtensibleVisitorTestGenerator extends TestGenerator with AbstractGenerator {
  val domain: BaseDomain with ModelDomain
  import domain._

  /** Add virtual type generator. */
  def addVirtualConstructor(mainType:TypeDeclaration[_], op:domain.Operation, className:String) : Unit = {
    val virtualConstructor = Java(
      s"""|${op.name.capitalize} make${op.name.capitalize} (${parameters(op)}) {
          |  return new $className (${arguments(op)});
          |}""".stripMargin).methodDeclarations().head

    mainType.addMember(virtualConstructor)
  }

  /** Combine all test cases together into a single JUnit 3.0 TestSuite class. */
  override def generateSuite(pkg: Option[String], m:Option[Model] = None): Seq[CompilationUnit] = {
    super.generateSuite(pkg, m).map(unit => {
      val lastModel = getModel.lastModelWithDataTypes()
      getModel.flatten().ops.foreach(op => {
        val className = if (lastModel.ops.contains(op)) {
          op.name.capitalize
        } else {
          op.name.capitalize + lastModel.types.sortWith(_.name < _.name).mkString("")
        }
        addVirtualConstructor(unit.getType(0), op, className)
      })
      unit
    })
  }
}
