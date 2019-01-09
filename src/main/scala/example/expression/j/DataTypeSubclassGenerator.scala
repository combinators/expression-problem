package example.expression.j  /*DI:LD:AI*/

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.expr.Expression
import example.expression.domain.ModelDomain
import org.combinators.templating.twirl.Java

/**
  * Some solutions have classes that are represented by a base class and then one class for
  * each of the known data types.
  */
trait DataTypeSubclassGenerator {
  val domain:ModelDomain

  /** Generate the full class for the given expression sub-type. */
  def generateExp(model:domain.Model, e:domain.Atomic) : CompilationUnit

  /** Generate the base class. */
  def generateBase(model:domain.Model) : CompilationUnit

  /**
    * Responsible for delegating to a new operation on the current data-type.
    */
  def delegateFixMe(exp:domain.Atomic, op:domain.Operation, params:Expression*) : Expression = {
    val opargs = params.mkString(",")
    Java(s"this.${op.name.toLowerCase}($opargs)").expression[Expression]()
  }

  /** For Java, same behavior as delegate. */
  def identify(exp:domain.Atomic, op:domain.Operation, params:Expression*) : Expression = {
    delegateFixMe(exp, op, params : _*)
  }
}
