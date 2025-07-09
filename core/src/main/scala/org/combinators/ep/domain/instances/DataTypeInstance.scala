package org.combinators.ep.domain.instances

/*DI:LI:AI*/

/** Provides abstractions used to represent type instances of the host language (Scala) in a domain specific context and
  * vice versa domain specific data types in the host language.
  */

import org.combinators.cogen.{InstanceRep, TypeRep}
import org.combinators.ep.domain.GenericModel
import org.combinators.ep.domain.abstractions.{DataTypeCase, DomainTpeRep}


/** Models instances of a domain specific data type in Scala.
  *
  * Types of the `attributeInstances` need to match the attribute types of the data type, which is also checked by
  * a runtime assertion.
  *
  * @param tpeCase            The domain type case to model.
  * @param attributeInstances Instances for all attributes of the domain type.
  */
case class DataTypeInstance(tpeCase: DataTypeCase, attributeInstances: Seq[InstanceRep]) {
  require({
    tpeCase.attributes.corresponds(attributeInstances) { (attribute, attributeInstance) =>
      attribute.tpe == attributeInstance.tpe
    }
  },
    "Attribute instances need to match the attribute types described in the type case"
  )

  /** Creates a new instance representation for the given domain specific data type instance using the
    * base data type of the implicitly given domain model.
    */
  def tpInstanceRep(implicit domain: GenericModel): InstanceRep = {
    InstanceRep(DomainTpeRep.DataType(domain.baseDataType))(this)
  }
}

object DataTypeInstanceRep {
  def apply(instance: DataTypeInstance)(implicit domain: GenericModel): InstanceRep = instance.tpInstanceRep
}