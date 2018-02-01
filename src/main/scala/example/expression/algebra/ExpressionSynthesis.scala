package example.expression.algebra

import com.github.javaparser.ast.CompilationUnit
import example.expression.ExpressionDomain
import expression._
import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types._
import org.combinators.templating.twirl.Java

import expression.data.Eval

import scala.collection.JavaConverters._

/** Future work to sanitize combinators to be independent of Exp. */
class ExpressionSynthesis(override val domain:DomainModel) extends ExpressionDomain(domain) with SemanticTypes {

  /** Generate from domain. USER NEEDS TO SPECIFY THESE EITHER AUTOMATICALLY OR MANUALLY */
  @combinator object BaseInterface {
    def apply() : CompilationUnit =
    Java(s"""
         |package algebra;
         |
         |// The evaluation interface
         |interface Eval {
         |	int eval();
         |}
         |""".stripMargin).compilationUnit()

    val semanticType:Type = ops(ops.base, new Eval)
  }
  @combinator object BaseExpClass {
    def apply() : CompilationUnit =
      Java(s"""
              interface ExpAlg<E> {
              |	E lit(int x);
              |	E add(E e1, E e2);
              |}
              |
              |""".stripMargin).compilationUnit()

    val semanticType:Type = exp(exp.base, new Exp)
  }
}
