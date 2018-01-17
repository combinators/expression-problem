package example.expression.cpp


import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types._

import example.expression.ExpressionDomain
import expression._

import scala.collection.JavaConverters._

/** Future work to sanitize combinators to be independent of Exp. */
class ExpressionSynthesis(override val domain:DomainModel) extends ExpressionDomain(domain) with CPPSemanticTypes {

  /** Construct visitor abstract class. */
  @combinator object Visitor {
    def apply: CPPClass = {
      val signatures = domain.data.asScala
            .map(x => s"""virtual void Visit${x.getClass.getSimpleName}(const ${x.getClass.getSimpleName}* e) = 0;""")

      /*
       * A concrete visitor describes a concrete operation on expressions. There is one visit
       * method per type in the class hierarchy.
       */
      new CPPClass("ExpVisitor", "ExpVisitor", signatures, Seq.empty)

    }

    val semanticType:Type = generated(generated.visitor)
  }

  /** Generic Expr base class with visitor. */
  @combinator object BaseExpClass {
    def apply: CPPClass = {

      new CPPClass("Exp", "Exp", Seq("virtual void Accept(ExpVisitor* visitor) const = 0;"), Seq.empty)
    }
    val semanticType:Type = exp(exp.base, new Exp)
  }
}
