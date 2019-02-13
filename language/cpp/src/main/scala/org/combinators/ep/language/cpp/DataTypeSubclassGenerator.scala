package ep.cpp

import org.combinators.ep.domain.ModelDomain

/*DI:LD:AI*/

/**
  * Some solutions have classes that are represented by a base class and then one class for
  * each of the known data types.
  */
trait DataTypeSubclassGenerator {
  val domain:ModelDomain

  /** Generate the full class for the given expression sub-type. */
  def generateExp(model:domain.Model, e:domain.Atomic) : CPPFile

  /** Generate the base class. */
  def generateBase(model:domain.Model) : CPPFile
}
