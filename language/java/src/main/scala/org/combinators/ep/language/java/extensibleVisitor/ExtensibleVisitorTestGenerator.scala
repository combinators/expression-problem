package org.combinators.ep.language.java.extensibleVisitor   /*DI:LD:AD*/

import com.github.javaparser.ast.body.TypeDeclaration
import org.combinators.ep.domain.BaseDomain
import org.combinators.ep.language.java.JUnitTestGenerator
import org.combinators.templating.twirl.Java

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait ExtensibleVisitorTestGenerator
  extends JUnitTestGenerator
    with ExtensibleVisitorGenerator {
  val domain: BaseDomain with ModelDomain

  /** Add virtual type generator. */
  def addVirtualConstructor(mainType:TypeDeclaration[_], op:domain.Operation, className:String) : Unit = {
    val virtualConstructor = Java(
      s"""|$className make${op.concept} (${parameters(op)}) {
          |  return new $className (${arguments(op)});
          |}""".stripMargin).methodDeclarations().head

    mainType.addMember(virtualConstructor)
  }

  /** Combine all test cases together into a single JUnit 3.0 TestSuite class. */
  override def generateSuite(pkg: Option[String]): Seq[CompilationUnit] = {
    super.generateSuite(pkg).map(unit => {

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
            val clazzName = if (m.base == m) {
              op.concept
            } else {
              op.concept + full
            }
            addVirtualConstructor(unit.getType(0), op, clazzName)  // ???? these are not qualified
          })
        } else {
          if (reached) {
            // now all of these use the lastTypes signature $full
            m.ops.foreach(op => {
              addVirtualConstructor(unit.getType(0), op, op.concept + full)
            })
          } else {
            m.ops.foreach(op => {
              addVirtualConstructor(unit.getType(0), op, op.concept) // newer and are allowed in as straight
            })
          }
        }
      })

      unit
    })
  }
}
