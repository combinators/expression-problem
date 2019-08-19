package org.combinators.ep.generator

import org.combinators.ep.domain.abstractions._

/** Provides mangled names for domain entities. */
abstract class NameProvider {
  /** Mangles `name` according to language specific rules. */
  def mangle(name: String): String

  /** Provides the name for a language representation of concepts (e.g. classes) associated with the given data type.
    * Most languages just want to mangle the capitalized data type name.
    */
  def conceptNameOf(dataType: DataType): String =
    mangle(dataType.name.capitalize)

  /** Provides the name for a language representation of concepts (e.g. classes) associated with the given data type
    * case.
    * Most languages just want to mangle the capitalized data type name.
    */
  def conceptNameOf(dataTypeCase: DataTypeCase): String =
    mangle(dataTypeCase.name.capitalize)

  /** Provides the name for a language representation of concepts (e.g. classes) associated with the given operation.
    * Most languages just want to mangle the capitalized operation name. */
  def conceptNameOf(operation: Operation): String =
    mangle(operation.name.capitalize)

  /** Provides the name for a language representation of concepts (e.g. classes) associated with the given type
    * representation.
    * Most languages just want to mangle the capitalized case name. */
  def conceptNameOf(tpe: TypeRep): String = {
    tpe match {
      case TypeRep.DataType(domainType) => conceptNameOf(domainType)
      case _ => mangle(tpe.getClass.getName.capitalize)
    }
  }

  /** Provides the name for a language representation of instances (e.g. objects) associated with the given data type.
    * Most languages just want to mangle the lower case data type name. */
  def instanceNameOf(dataType: DataType): String =
    mangle(dataType.name.toLowerCase)

  /** Provides the name for a language representation of instances (e.g. objects) associated with the given data type
    * case.
    * Most languages just want to mangle the lower case name of the case. */
  def instanceNameOf(dataTypeCase: DataTypeCase): String =
    mangle(dataTypeCase.name.toLowerCase)

  /** Provides the name for a language representation of instances (e.g. method definitions) associated with
    * the given operation.
    * Most languages just want to mangle the lower case operation name. */
  def instanceNameOf(operation: Operation): String =
    mangle(operation.name.toLowerCase)

  /** Provides the name for a language representation of instances (e.g. objects) associated with the given type
    * representation.
    * Most languages just want to mangle the capitalized type name. */
  def instanceNameOf(tpe: TypeRep): String = {
    tpe match {
      case TypeRep.DataType(domainType) => instanceNameOf(domainType)
      case _ => mangle(tpe.getClass.getName.toLowerCase)
    }
  }
}
