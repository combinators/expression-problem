package org.combinators.models

// Nice use of overloading to simplify the expressions
trait Expression {
  def +(other: Expression): Expression = AdditionExpression(this, other)
  def -(other: Expression): Expression = SubtractionExpression(this, other)
  def *(other: Expression): Expression = MultiplicationExpression(this, other)
  def /(other: Expression): Expression = DivisionExpression(this, other)

  def <(other: Expression): Expression & BooleanExpression = LessThanExpression(this, other)
  def <=(other: Expression): Expression & BooleanExpression = LessThanOrEqualExpression(this, other)
  def >(other: Expression) : Expression & BooleanExpression = LessThanExpression(other, this)                // opposite
  def >=(other: Expression) : Expression & BooleanExpression = LessThanOrEqualExpression(other, this)        // opposite
  
  // When using ==, must assume it is IntegerType: Dangerous?? todo: allow for other types(?)
  def ==(other: Expression): Expression & BooleanExpression = EqualExpression(this, other, IntegerType())
  def ||(other: Expression): Expression & BooleanExpression = OrExpression(this, other)
  def &&(other: Expression): Expression & BooleanExpression = AndExpression(this, other)

  def apply(other: Expression): Expression = ArrayElementExpression(this, other)
  def tpe:ArgumentType
}

// tagging an expression as returning a Boolean value, which means it can be used in IfElseExpression
trait BooleanExpression {
  def tpe:ArgumentType = BooleanType()
}

/**
 * Necessary for defining what will be either Input or Output of a test case
 * 
 */
trait LiteralExpression

class UnitExpression extends LiteralExpression {
  def tpe: ArgumentType = UnitType()
}   // VOID

//Integer
case class AdditionExpression(left: Expression, right: Expression) extends Expression {
  def tpe: ArgumentType = {
    val leftType = left.tpe
    val rightType = right.tpe
    assert (leftType == rightType, "Mismatched types in Expression")
    leftType
  }
}
case class SubtractionExpression(left: Expression, right: Expression) extends Expression {
  def tpe: ArgumentType = {
    val leftType = left.tpe
    val rightType = right.tpe
    assert (leftType == rightType, "Mismatched types in Expression")
    leftType
  }
}
case class MultiplicationExpression(left: Expression, right: Expression) extends Expression {
  def tpe: ArgumentType = {
    val leftType = left.tpe
    val rightType = right.tpe
    assert (leftType == rightType, "Mismatched types in Expression")
    leftType
  }
}
case class DivisionExpression(left: Expression, right: Expression) extends Expression {
  def tpe: ArgumentType = {
    val leftType = left.tpe
    val rightType = right.tpe
    assert (leftType == rightType, "Mismatched types in Expression")
    leftType
  }
}

// Vast majority are IntegerType
case class SubproblemExpression(args: Seq[Expression], argType:ArgumentType = IntegerType()) extends Expression {
  def tpe: ArgumentType = argType
}

// If helper is defined but NOT part of the parameters during invocation, then it must be passed in as helpers
case class SubproblemInvocation(
       order:Seq[String],
       helpers:Map[String,HelperExpression] = Map.empty,     // known variables that are used in the problem expansion without being iterated over or called
       returnType: ArgumentType = IntegerType(),
       mappers: Map[String, Expression] = Map.empty)         // variables that map to new coordinates into dp[] space and are added to bottom up

case class MaxExpression(left: Expression, right: Expression) extends Expression {
  def tpe: ArgumentType = {
    val leftType = left.tpe
    val rightType = right.tpe
    assert (leftType == rightType, "Mismatched types in Expression")
    leftType
  }
}
case class MinExpression(left: Expression, right: Expression) extends Expression {
  def tpe: ArgumentType = {
    val leftType = left.tpe
    val rightType = right.tpe
    assert (leftType == rightType, "Mismatched types in Expression")
    leftType
  }
}

case class ArrayElementExpression(array: Expression, index: Expression) extends Expression {
  def tpe:ArgumentType = {
    assert(array.tpe.isInstanceOf[org.combinators.models.ArrayType], "ArrayElementExpression needs an array as first argument.")
    array.tpe.asInstanceOf[org.combinators.models.ArrayType].elementType
  }
}

// TODO: Unused though could be used in future
case class FunctionExpression(name:String, args: Seq[Expression]) extends Expression {
  def tpe:ArgumentType = IntegerType()
}

case class LiteralInt(literal: Int) extends LiteralExpression with Expression {
  def tpe:ArgumentType = IntegerType()
}
case class IteratorExpression(iteratorNumber: Int, variable:String) extends Expression {
  def tpe: ArgumentType = IntegerType()
}

case class CharToAsciiExpression(char: Expression) extends Expression {
  def tpe:ArgumentType = IntegerType()
}

// low and high are INCLUSIVE
case class HelperExpression(variable:String,
                            low:Expression,
                            in_range:Expression,
                            high:Expression) extends Expression {
  def tpe:ArgumentType = IntegerType()
}

case class PackedArrayType(elementType:ArgumentType) extends ArgumentType

/** This remains a class (not case class) because of the auxiliary constructor (to play nice with IntelliJ editor) */
class LiteralArray(val literal:Array[Int], val dimensions:Seq[Int]) extends LiteralExpression {

  // Auxiliary constructor to handle situation where dimensions is not provided: Just defaults to a one-dimension array of given length.
  def this(literal:Array[Int]) = {
    this(literal, Seq(literal.length))
  }

  def tpe:ArgumentType = PackedArrayType(IntegerType())
}

// For when a HelpExpression needs to refer to self
// TODO: pass in
case class SelfExpression(variableName:String) extends Expression {
  def tpe:ArgumentType = ???
}

case class StringLengthExpression(string: Expression) extends Expression {
  def tpe:ArgumentType = IntegerType()
}
case class SubStringExpression(string:Expression, start:Expression, exclusiveEnd:Expression) extends Expression {
  def tpe:ArgumentType = StringType()
}
case class ArrayLengthExpression(array: Expression) extends Expression {
  def tpe:ArgumentType = IntegerType()
}

//String
case class LiteralString(literal: String) extends LiteralExpression {
  def tpe:ArgumentType = StringType()
}

//Character
case class LiteralChar(literal:Char) extends LiteralExpression with Expression {
  def tpe:ArgumentType = CharType()
}
case class CharAtExpression(string: Expression, index: Expression) extends Expression {
  def tpe:ArgumentType = CharType()
}

// Access field access for the primary class
// TODO: OLD and deal with later
case class InputExpression(variableName:String) extends Expression {
  def tpe:ArgumentType = ???
}

case class EqualExpression(left: Expression, right: Expression, argType:ArgumentType = org.combinators.models.IntegerType()) extends Expression with BooleanExpression
case class OrExpression(left: Expression, right: Expression) extends Expression with BooleanExpression
case class AndExpression(left: Expression, right: Expression) extends Expression with BooleanExpression
case class LessThanExpression(left: Expression, right:Expression) extends Expression with BooleanExpression
case class LessThanOrEqualExpression(left: Expression, right:Expression) extends Expression with BooleanExpression
case class TernaryExpression(condition: Expression & BooleanExpression, trueBranch: Expression, falseBranch: Expression) extends Expression {
  def tpe:ArgumentType = {
    val trueType = trueBranch.tpe
    val falseType = falseBranch.tpe
    assert(trueType == falseType, "Mismatched types in TernaryExpression")
    trueType
  }
}

case class LiteralBoolean(literal:Boolean) extends LiteralExpression with Expression with BooleanExpression

// Now includes the name of the int variable to iterate over
case class ArgExpression(whichArg: Int, name:String, argType:ArgumentType, itArgName:String) extends Expression {
  def tpe:ArgumentType = argType
}

