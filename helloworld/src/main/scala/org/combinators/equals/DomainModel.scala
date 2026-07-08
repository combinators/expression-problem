package org.combinators.equals

import org.combinators.cogen.TypeRep

/** Represents a Scala model of an instance of the given domain specific data type. */

// need downcast from Object to the class
// for each field, knows == or .equals or Arrays.equals()
// if not in-built type, then we are declaring so must rely on .equals()

trait DataType {
  
}

case class CompositeDataType(val name:String, fields:Map[String, DataType]) extends DataType {
  
}

case class BuiltInDataType(val typeRep:TypeRep) extends DataType {
  
}

// Not so clear how to make arrays work, especially with non-built in types.
case class ArrayDataType(val typeRep:TypeRep) extends DataType {

}

