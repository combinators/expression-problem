package example.expression.j

import com.github.javaparser.ast.CompilationUnit
import example.expression.domain.Domain

/**
  * Some solutions have classes that are represented by a base class and then one class for
  * each of the known data types.
  */
trait DataTypeSubclassGenerator {
  val domain:Domain
  import domain._

  // Serendipitous that both Straight and Visitor have a need for these two.
  // note that Visitor adds 'operationGenerator' as a CompilationUnit. Perhaps we can
  // take advantage of extracting even these two outside...
  // indeed. Perhaps we could even replace with call to process(Model*):Seq[CompilationUnit]
  // on each of the successive models. That is, process(e0,e1) or process(e0,e1,e2,e3)

  /** Generate the full class for the given expression sub-type. */
  def generateExp(domain:Model, e:expressions.Exp) : CompilationUnit

  /** Generate the base class. */
  def generateBase(domain:Model) : CompilationUnit
}






