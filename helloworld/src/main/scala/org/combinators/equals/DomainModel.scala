package org.combinators.equals

import org.combinators.cogen.{InstanceRep, TypeRep}
import org.combinators.equals.ffi.BaseType


/** Represents a composite data type, composed either from built-in data types, or other composite data types. */
case class CompositeDataType(val name:String, fields:Seq[(String, TypeRep)]) {
  def inst(args: Seq[(String, InstanceRep)]): InstanceRep = InstanceRep(BaseType.CompositeTpe(this))(args)
}
case class EqualsTestCase(val object1:InstanceRep, val object2:InstanceRep, val expected:Boolean)

/** Sample Domain. */
object ShapeDomain {
   
  val point: CompositeDataType = CompositeDataType(
    name = "Point",
    fields = Seq("x" -> TypeRep.Int, "y" -> TypeRep.Int)
  )

  val pointTypeRep: TypeRep.OfHostType[Seq[(String, InstanceRep)]] = BaseType.CompositeTpe(point)
  val pt0 = Seq("x" -> InstanceRep(TypeRep.Int)(1), "y" -> InstanceRep(TypeRep.Int)(2))
  val pt1 = Seq("x" -> InstanceRep(TypeRep.Int)(1), "y" -> InstanceRep(TypeRep.Int)(2))
  val pt2 = Seq("x" -> InstanceRep(TypeRep.Int)(2), "y" -> InstanceRep(TypeRep.Int)(3))
  val pt3 = Seq("x" -> InstanceRep(TypeRep.Int)(1), "y" -> InstanceRep(TypeRep.Int)(3))

  
  val pointArray = TypeRep.Array[Seq[(String, InstanceRep)]](pointTypeRep)
  val rectangle: CompositeDataType = CompositeDataType(
    name = "Rectangle",
    fields = Seq(
      "height" -> TypeRep.Int,
      "width" -> TypeRep.Int,
      "anchors" -> pointArray
    )
  )

  val ar1 = InstanceRep(pointArray)(Array(pt1, pt2))
  val ar2 = InstanceRep(pointArray)(Array(pt2, pt3))

  val rect0 = Seq("height" -> InstanceRep(TypeRep.Int)(5), "width" -> InstanceRep(TypeRep.Int)(10), "anchors" -> ar1)
  val rect1 = Seq("height" -> InstanceRep(TypeRep.Int)(5), "width" -> InstanceRep(TypeRep.Int)(10), "anchors" -> ar1)
  val rect2 = Seq("height" -> InstanceRep(TypeRep.Int)(5), "width" -> InstanceRep(TypeRep.Int)(15), "anchors" -> ar1)
  val rect3 = Seq("height" -> InstanceRep(TypeRep.Int)(5), "width" -> InstanceRep(TypeRep.Int)(10), "anchors" -> ar2)
  
  // test cases here
  val testCases = Seq(
    EqualsTestCase(point.inst(pt0), point.inst(pt1), true),
    EqualsTestCase(rectangle.inst(rect0), rectangle.inst(rect1), true),
    EqualsTestCase(point.inst(pt1), point.inst(pt2), false),
    EqualsTestCase(rectangle.inst(rect1), rectangle.inst(rect2), false),
    EqualsTestCase(point.inst(pt1), rectangle.inst(rect1), false),
    EqualsTestCase(rectangle.inst(rect1), point.inst(pt1), false),
  )
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