package org.combinators.equals

import org.combinators.cogen.TypeRep
import ffi.BaseType.CompositeTpe

/** Represents a Scala model of an instance of the given domain specific data type. */

trait DataType {
  
}

/** Represents a composite data type, composed either from built-in data types, or other composite data types. */
case class CompositeDataType(val name:String, fields:Map[String, DataType]) extends DataType {
  
}

/** Represents a standard built in data type (like Int or String). */
case class BuiltInDataType(val typeRep:TypeRep) extends DataType {
  
}

trait BaseObjectType { }
case class PrimitiveInt(val value:Int) extends BaseObjectType { }
case class PrimitiveString(val value:String) extends BaseObjectType { }

case class ObjectType(val name: String, val fields: Map[String, BaseObjectType]) extends BaseObjectType { }

case class ArrayType(val tpe:String, val dimensions: Seq[Int], val values:Seq[BaseObjectType]) extends BaseObjectType { }

case class EqualsTestCase(val tpe:String, val object1:ObjectType, val object2:ObjectType, val expected:Boolean)

/** Sample Domain. */
object ShapeDomain {
  val point: CompositeDataType = new CompositeDataType(
    name = "Point",
    fields = Map("x" -> BuiltInDataType(TypeRep.Int), "y" -> BuiltInDataType(TypeRep.Int))
  )

  val pt0 = ObjectType("Point", Map("x" -> PrimitiveInt(1), "y" -> PrimitiveInt(2)))
  val pt1 = ObjectType("Point", Map("x" -> PrimitiveInt(1), "y" -> PrimitiveInt(2)))
  val pt2 = ObjectType("Point", Map("x" -> PrimitiveInt(2), "y" -> PrimitiveInt(3)))
  val pt3 = ObjectType("Point", Map("x" -> PrimitiveInt(1), "y" -> PrimitiveInt(3)))

  val pointTypeRep: TypeRep.OfHostType[Map[String, Any]] = CompositeTpe(point)
  val pointArray = TypeRep.Array[Map[String, Any]](CompositeTpe(point))

  val rectangle: CompositeDataType = new CompositeDataType(
    name = "Rectangle",
    fields = Map("height" -> BuiltInDataType(TypeRep.Int), "width" -> BuiltInDataType(TypeRep.Int),
      "anchors" -> BuiltInDataType(pointArray))
  )

  val ar1 = ArrayType("Point", Seq(1), Array(pt1, pt2))
  val ar2 = ArrayType("Point", Seq(1), Array(pt2, pt3))

  val rect0 = ObjectType("Rectangle", Map("height" -> PrimitiveInt(5), "width" -> PrimitiveInt(10), "anchors" -> ar1))  
  val rect1 = ObjectType("Rectangle", Map("height" -> PrimitiveInt(5), "width" -> PrimitiveInt(10), "anchors" -> ar1))
  val rect2 = ObjectType("Rectangle", Map("height" -> PrimitiveInt(5), "width" -> PrimitiveInt(15), "anchors" -> ar1))
  val rect3 = ObjectType("Rectangle", Map("height" -> PrimitiveInt(5), "width" -> PrimitiveInt(10), "anchors" -> ar2))
  
  // test cases here
  val testCases = Seq(EqualsTestCase("Point", pt0, pt1, true), 
    EqualsTestCase("Rectangle", rect0, rect1, true),
    EqualsTestCase("Point", pt1, pt2, false), 
    EqualsTestCase("Rectangle", rect1, rect2, false))
}

// note: at no point do we need to discuss arrays, because: FFI CoGen handles array equality correctly.
// Hidden benefits of FFIs as well as CoCo design pattern. 

// to get working with Scala, still would need to have InBetween. 
//
// by placing in inbetween. We have imperative API for generating AST.
// java bridges gap manually because of java github parser.
// for inbetween we bridge that gap once (generically). Then for each AST node, tell it how to translate that AST 
// node into the target language. No longer have to connect EACH and EVERY command to the AST and you can focus 
// only on the differences.