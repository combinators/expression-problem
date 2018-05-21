package example.expression.j

import expression.Exp
import expression.data.Add
import expression.extensions.{Divd, Mult, Neg, Sub}
import expression.types.{FrameworkTypes, GenericType, TypeInformation, Types}

trait Operators {

  /** Convert a type into its Java String equivalent. */
  def Type_toString (ty:TypeInformation): String =
    ty match {
      case Types.Exp=> "Exp"           // base class of everything

      case Types.Void => "void"
      case Types.Double => "Double"      // allow boxing/unboxing for generics
      case Types.Int => "Integer"      // allow boxing/unboxing for generics
      case Types.String => "String"
      case g:GenericType => Type_toString(g.base) + "<" + Type_toString(g.generic) + ">"
      case FrameworkTypes.List => "java.util.List"
      case _ => "None"
    }

//  /**
//    * Return the operator to use for a given binary exp type in Java.
//    *
//    * @param exp Exp subclass that corresponds to a binary operator.
//    * @return
//    */
//  def getBinaryOperator( exp:Exp) : com.github.javaparser.ast.expr.BinaryExpr.Operator = {
//    exp match {
//
//      case _:Add => com.github.javaparser.ast.expr.BinaryExpr.Operator.PLUS
//      case _:Sub => com.github.javaparser.ast.expr.BinaryExpr.Operator.MINUS
//      case _:Mult => com.github.javaparser.ast.expr.BinaryExpr.Operator.MULTIPLY
//      case _:Divd => com.github.javaparser.ast.expr.BinaryExpr.Operator.DIVIDE
//
//      case _ => null
//    }
//  }
//
//  /**
//    * Return the operator to use for a given unary exp type in Java.
//    *
//    * @param exp  Exp subclass that corresponds to a unary operator.
//    * @return
//    */
//  def getUnaryOperator( exp:Exp) : com.github.javaparser.ast.expr.UnaryExpr.Operator = {
//    exp match {
//
//      case _:Neg => com.github.javaparser.ast.expr.UnaryExpr.Operator.MINUS
//
//      case _ => null
//    }
//  }

}
