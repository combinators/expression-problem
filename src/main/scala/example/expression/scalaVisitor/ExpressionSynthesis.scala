package example.expression.scalaVisitor

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.expr.SimpleName
import com.github.javaparser.ast.stmt.Statement
import example.expression.domain.Domain
import example.expression.j.Operators
import expression.instances.{UnitSuite, UnitTest}
import org.combinators.templating.twirl.Java
import shared.compilation.CodeGeneratorRegistry

import scala.collection.JavaConverters._

/** Future work: reduce the need to have full set of traits here, with the internals accessing the code generators. Separate concerns */
trait ExpressionSynthesis extends Operators with Domain {

  def testCaseGenerator(op:Operation, identifier:SimpleName, tc: UnitTest) : Seq[Statement]

//
//  /** Generate from domain. USER NEEDS TO SPECIFY THESE EITHER AUTOMATICALLY OR MANUALLY */
//  def BaseExpClass(): CompilationUnit =
//      Java(s"""|package expression;
//               |
//               |public abstract class Exp {
//               |    public abstract <R> R accept(Visitor<R> v);
//               |}
//               |""".stripMargin).compilationUnit()

//
//  /** Works on any subclass of Exp to produce the base class structure for a sub-type of Exp. */
//  def BaseClass(expr:types.Types):CompilationUnit = {
//
//    val name = expr.getClass.getSimpleName
//    Java(s"""package expression; public class $name extends Exp { }""".stripMargin).compilationUnit()
//  }


}
