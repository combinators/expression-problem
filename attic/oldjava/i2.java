package org.combinators.ep.language.java;

class i2 {
// GOING AWAY
}
///**
//  * Independent branch to hold Power and Height.
//  *
//  * By definition, an empty tree has height -1. A tree with a single root node has height 0.
//  */
//trait i2 extends  Evolution with DomainIndependentJavaGenerator with JUnitTestGenerator with I2 {
//  self: e0 with e1 with i1 =>
//
//  abstract override def typeConverter(tpe:domain.TypeRep) : com.github.javaparser.ast.`type`.Type = {
//    tpe match {
//      case Integer => Java(s"Integer").tpe()
//      case _ => super.typeConverter(tpe)
//    }
//  }
//
//   abstract override def logic(exp:domain.DataType, op:domain.Operation): Seq[Statement] = {
//    // generate the actual body
//    op match {
//      case Height =>
//        val heightPlusOne:Expression = Java(s"${independent.height} + 1").expression()
//        exp match {
//          case _:domain.Binary => result(Java(s"Math.max(${dispatch(expression(exp, domain.base.left), Height, heightPlusOne)},${dispatch(expression(exp, domain.base.right), Height, heightPlusOne)}) ").expression())
//          case _:domain.Unary => result(Java(s"${dispatch(expression(exp, domain.base.inner), Height, heightPlusOne)}").expression())
//          case _:domain.Atomic => result(Java(s" ${independent.height}").expression())
//
//          case _ => super.logic(exp, op)
//        }
//
//      case _ => super.logic(exp, op)
//    }
//  }
//
//  // TODO: CONVERT TO TEST CASES...
//  abstract override def testGenerator: Seq[MethodDeclaration] = {
//    val i1 = new domain.UnaryInst(Inv, LitInst(2.0))
//    val a1 = new domain.BinaryInst(Add, LitInst(5.0), LitInst(7.0))
//    val a2 = new domain.BinaryInst(Add, LitInst(2.0), LitInst(3.0))
//    val i2 = new domain.UnaryInst(Inv, a2)
//    val a3 = new domain.BinaryInst(Add, a1, i2)
//    val zero = Java("0").expression[Expression]()
//
//    val testBlock =
//      actual(Height, i1, zero).appendDependent { case Seq(i1Inst) =>
//          actual(Height, a3, zero).appendDependent { case Seq(a3Inst) =>
//              CodeBlockWithResultingExpressions(
//                Java(
//                  s"""
//                     |assertEquals(new Integer(1), $i1Inst);
//                     |assertEquals(new Integer(3), $a3Inst);
//                   """.stripMargin).statements():_*
//              )()
//          }
//      }.block
//
//    super.testGenerator ++ Java(
//      s"""
//         |public void test() {
//         |   ${testBlock.mkString("\n")}
//         |}""".stripMargin).methodDeclarations()
//  }
//}
