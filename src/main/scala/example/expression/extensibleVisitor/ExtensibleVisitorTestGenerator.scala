package example.expression.extensibleVisitor    /*DI:LD:AD*/

import com.github.javaparser.ast.body.TypeDeclaration
import example.expression.domain.{BaseDomain, ModelDomain}
import example.expression.j.{JavaGenerator, JUnitTestGenerator}
import org.combinators.templating.twirl.Java

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait ExtensibleVisitorTestGenerator extends JUnitTestGenerator with JavaGenerator {
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

      // get all operations (via flatten). Then find the most recent model that has types
      // defined. All operations that come *AFTER* have no trailing suffix. All operations
      // that were defined BEFORE must use those types as the suffix
      val lastTypes = getModel.lastModelWithDataTypes()
      val full = lastTypes.types.sortWith(_.name < _.name).mkString("")

      // in reverse chrono order
      var reached:Boolean = false
      getModel.toSeq.foreach (m => {
        if (m == lastTypes) {
          reached = true
          m.ops.foreach(op => {
            addVirtualConstructor(unit.getType(0), op, op.name.capitalize)  // these are not qualified
          })
        } else {
          if (reached) {
            // now all of these use the lastTypes signature $full
            m.ops.foreach(op => {
              addVirtualConstructor(unit.getType(0), op, op.name.capitalize + full)
            })
          } else {
            m.ops.foreach(op => {
              addVirtualConstructor(unit.getType(0), op, op.name.capitalize) // newer and are allowed in as straight
            })
          }
        }
      })

      unit
    })
  }
}
