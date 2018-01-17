package example.expression.covariant

import com.github.javaparser.ast.CompilationUnit
import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types._
import org.combinators.templating.twirl.Java
import example.expression.ExpressionDomain
import expression._

import scala.collection.JavaConverters._

/** Future work to sanitize combinators to be independent of Exp. */
class ExpressionSynthesis(override val domain:DomainModel) extends ExpressionDomain(domain) with SemanticTypes {

  /** Construct visitor abstract class. */
  @combinator object Visitor {
    def apply(): CompilationUnit = {
      val signatures = domain.data.asScala
            .map(x => s"""public abstract R visit(${x.getClass.getSimpleName} exp);""").mkString("\n")

      Java (s"""
           |package expression;
           |/*
           | * A concrete visitor describes a concrete operation on expressions. There is one visit
           | * method per type in the class hierarchy.
           | */
           |public abstract class Visitor<R> {
           |
           |$signatures
           |}
         """.stripMargin).compilationUnit()
    }

    val semanticType:Type = generated(generated.visitor)
  }

  /** Generate from domain. USER NEEDS TO SPECIFY THESE EITHER AUTOMATICALLY OR MANUALLY */
  @combinator object BaseExpClass {
    def apply() : CompilationUnit =
    Java(s"""
         |package expression;
         |
         |public abstract class Exp {
         |    public abstract <R> R accept(Visitor<R> v);
         |}
         |""".stripMargin).compilationUnit()

    val semanticType:Type = exp(exp.base, new Exp)
  }
}
