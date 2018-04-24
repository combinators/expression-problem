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


}
