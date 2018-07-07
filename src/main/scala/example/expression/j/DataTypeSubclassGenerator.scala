package example.expression.j

import com.github.javaparser.ast.CompilationUnit
import example.expression.domain.ModelDomain

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
}
