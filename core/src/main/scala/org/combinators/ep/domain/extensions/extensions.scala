package org.combinators.ep.domain.extensions

import org.combinators.cogen.{InstanceRep, NameProvider, TypeRep}
import org.combinators.ep.domain.GenericModel
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.domain.instances.DataTypeInstance

extension [Name](nameProvider: NameProvider[Name]) {
  /** Provides the name for a language representation of concepts (e.g. classes) associated with the given Model.
    * Most languages just want to return the capitalized model name.
    */
  def conceptNameOf(model: GenericModel): String =
    model.name.capitalize

  /** Provides the name for a language representation of concepts (e.g. classes) associated with the given data type.
    * Most languages just want to return the capitalized data type name.
    */
  def conceptNameOf(dataType: DataType): String =
    dataType.name.capitalize

  /** Provides the name for a language representation of concepts (e.g. classes) associated with the given data type
    * case.
    * Most languages just want to return the capitalized data type name.
    */
  def conceptNameOf(dataTypeCase: DataTypeCase): String =
    dataTypeCase.name.capitalize

  /** Provides the name for a language representation of concepts (e.g. classes) associated with the given operation.
    * Most languages just want to mangle the capitalized operation name. */
  def conceptNameOf(operation: Operation): String =
    operation.name.capitalize

  /** Provides the name for a language representation of concepts (e.g. attributes) associated with the given data type
    * case.
    * Most languages just want to return the lower case name of the case. */
  def conceptNameOf(att: Attribute): String =
    att.name.capitalize

  /** Provides the name for a language representation of concepts (e.g. classes) associated with the given type
    * representation.
    * Most languages just want to return the capitalized case name. */
  def conceptNameOf(tpe: TypeRep): String = {
    tpe match {
      case DomainTpeRep.DataType(domainType) => conceptNameOf(domainType)
      case _ => tpe.getClass.getName.capitalize
    }
  }

  /** Provides the name for a language representation of instances (e.g. objects) associated with the given model.
    * Most languages just want to return the lower case model name. */
  def instanceNameOf(model: GenericModel): String =
    model.name.toLowerCase

  /** Provides the name for a language representation of instances (e.g. objects) associated with the given data type.
    * Most languages just want to return the lower case data type name. */
  def instanceNameOf(dataType: DataType): String =
    dataType.name.toLowerCase

  /** Provides the name for a language representation of instances (e.g. objects) associated with the given data type
    * case.
    * Most languages just want to return the lower case name of the case. */
  def instanceNameOf(dataTypeCase: DataTypeCase): String =
    dataTypeCase.name.toLowerCase

  /** Provides the name for a language representation of instances (e.g. method definitions) associated with
    * the given operation.
    * Most languages just want to return the lower case operation name. */
  def instanceNameOf(operation: Operation): String =
    operation.name.toLowerCase

  /** Provides the name for a language representation of instances (e.g. attributes) associated with the given data type
    * case.
    * Most languages just want to return the lower case name of the case. */
  def instanceNameOf(att: Attribute): String =
    att.name.toLowerCase

  /** Provides the name for a language representation of instances (e.g. objects) associated with the given type
    * representation.
    * Most languages just want to return the capitalized type name. */
  def instanceNameOf(tpe: TypeRep): String = {
    tpe match {
      case DomainTpeRep.DataType(domainType) => instanceNameOf(domainType)
      case _ => tpe.getClass.getName.toLowerCase
    }
  }
}

extension (tpe: TypeRep) {
  def isModelBase(model:GenericModel):Boolean = tpe == DomainTpeRep.DataType(model.baseDataType)
}
