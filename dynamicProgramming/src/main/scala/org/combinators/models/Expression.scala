package org.combinators.models

// Nice use of overloading to simplify the expressions!!!
trait Expression {
  def +(other: Expression): Expression = new AdditionExpression(this,other)
  def -(other: Expression): Expression = new SubtractionExpression(this,other)
  def *(other: Expression): Expression = new MultiplicationExpression(this,other)
  def /(other: Expression): Expression = new DivisionExpression(this,other)

  def <(other: Expression): Expression & BooleanExpression = new LessThanExpression(this,other)
  def <=(other: Expression): Expression & BooleanExpression = new LessThanOrEqualExpression(this,other)

  // When using ==, must assume it is IntegerType: Dangerous?? todo: allow for other types(?)
  def ==(other: Expression): Expression & BooleanExpression = new EqualExpression(this,other, new IntegerType())
  def ||(other: Expression): Expression & BooleanExpression = new OrExpression(this,other)
  def &&(other: Expression): Expression & BooleanExpression = new AndExpression(this,other)

  def apply(other: Expression): Expression = new ArrayElementExpression(this, other)
  def tpe:ArgumentType
}

// tagging an expression as returning a Boolean value, which means it can be used in IfElseExpression
trait BooleanExpression {
  def tpe:ArgumentType = BooleanType()
}

// necessary for defining literals that form the input or possible output
trait LiteralExpression extends Expression

class UnitExpression extends LiteralExpression {
  def tpe: ArgumentType = UnitType()
}   // VOID

//Integer
class AdditionExpression(val left: Expression, val right: Expression) extends Expression {
  def tpe: ArgumentType = {
    val leftType = left.tpe
    val rightType = right.tpe
    assert (leftType == rightType, "Mismatched types in Expression")
    leftType
  }
}
class SubtractionExpression(val left: Expression, val right: Expression) extends Expression {
  def tpe: ArgumentType = {
    val leftType = left.tpe
    val rightType = right.tpe
    assert (leftType == rightType, "Mismatched types in Expression")
    leftType
  }
}
class MultiplicationExpression(val left: Expression, val right: Expression) extends Expression {
  def tpe: ArgumentType = {
    val leftType = left.tpe
    val rightType = right.tpe
    assert (leftType == rightType, "Mismatched types in Expression")
    leftType
  }
}
class DivisionExpression(val left: Expression, val right: Expression) extends Expression {
  def tpe: ArgumentType = {
    val leftType = left.tpe
    val rightType = right.tpe
    assert (leftType == rightType, "Mismatched types in Expression")
    leftType
  }
}

// Vast majority are IntegerType
class SubproblemExpression(val args: Seq[Expression], argType:ArgumentType = IntegerType()) extends Expression {
  def tpe: ArgumentType = argType
}

// If helper is defined but NOT part of the parameters during invocation, then it must be passed in as helpers
case class SubproblemInvocation(
       order:Seq[String],
       helpers:Map[String,HelperExpression] = Map.empty,     // known variables that are used in the problem expansion without being iterated over or called
       returnType: ArgumentType = IntegerType(),
       mappers: Map[String, Expression] = Map.empty)         // variables that map to new coordinates into dp[] space and are added to bottom up

class MaxExpression(val left: Expression, val right: Expression) extends Expression {
  def tpe: ArgumentType = {
    val leftType = left.tpe
    val rightType = right.tpe
    assert (leftType == rightType, "Mismatched types in Expression")
    leftType
  }
}
class MinExpression(val left: Expression, val right: Expression) extends Expression {
  def tpe: ArgumentType = {
    val leftType = left.tpe
    val rightType = right.tpe
    assert (leftType == rightType, "Mismatched types in Expression")
    leftType
  }
}

class ArrayElementExpression(val array: Expression, val index: Expression) extends Expression {
  def tpe:ArgumentType = {
    assert(array.tpe.isInstanceOf[org.combinators.models.ArrayType], "ArrayElementExpression needs an array as first argument.")
    array.tpe.asInstanceOf[org.combinators.models.ArrayType].elementType
  }
}

// TODO: pass in type
class FunctionExpression(val name:String, val args: Seq[Expression]) extends Expression {
  def tpe:ArgumentType = IntegerType()
}

class LiteralInt(val literal: Int) extends LiteralExpression {
  def tpe:ArgumentType = IntegerType()
}
class IteratorExpression(val iteratorNumber: Int, val variable:String) extends Expression {
  def tpe: ArgumentType = IntegerType()
}

class CharToAsciiExpression(val char: Expression) extends Expression {
  def tpe:ArgumentType = IntegerType()
}

// low and high are INCLUSIVE
case class HelperExpression(variable:String,
                            low:Expression,
                            in_range:Expression,
                            high:Expression) extends Expression {
  def tpe:ArgumentType = IntegerType()
}

// when input problem has two integers, not easily translated as (row, column)
case class LiteralPairType() extends ArgumentType
class LiteralPair(val val1:Int, val val2:Int) extends LiteralExpression {
  def tpe:ArgumentType = LiteralPairType()
}
case class LiteralTripleType() extends ArgumentType
class LiteralTriple(val val1:Int, val val2:Int, val val3:Int) extends LiteralExpression {
  def tpe: ArgumentType = LiteralTripleType()
}

// when dimensions is Seq.empty, then this is a 1D array, whose length is determined by the number of integers. Otherwise
// the sequence describes the dimensions
case class PackedArrayType(elementType:ArgumentType) extends ArgumentType
class LiteralArray(val literal:Array[Int], val dimensions:Seq[Int] = Seq(1)) extends LiteralExpression {
  def tpe:ArgumentType = PackedArrayType(IntegerType())
}

case class StringPairType() extends ArgumentType
class LiteralStringPair(val string1:String, val string2:String) extends LiteralExpression {
  def tpe:ArgumentType = StringPairType()
}
case class StringTripleType() extends ArgumentType
class LiteralStringTriple(val string1:String, val string2:String, val string3:String) extends LiteralExpression {
  def tpe:ArgumentType = StringTripleType()
}

case class IntegerArrayPair() extends ArgumentType
class LiteralArrayPair(val ar1:Array[Int], val ar2:Array[Int]) extends LiteralExpression {
  def tpe:ArgumentType = IntegerArrayPair()
}

// For when a HelpExpression needs to refer to self
// TODO: pass in
case class SelfExpression(variableName:String) extends Expression {
  def tpe:ArgumentType = ???
}

class StringLengthExpression(val string: Expression) extends Expression {
  def tpe:ArgumentType = IntegerType()
}
class SubStringExpression(val string:Expression, val start:Expression, val exclusiveEnd:Expression) extends Expression {
  def tpe:ArgumentType = StringType()
}
class ArrayLengthExpression(val array: Expression) extends Expression {
  def tpe:ArgumentType = IntegerType()
}

//String
class LiteralString(val literal: String) extends LiteralExpression {
  def tpe:ArgumentType = StringType()
}

//Character
class LiteralChar(val literal:Char) extends LiteralExpression {
  def tpe:ArgumentType = CharType()
}
class CharAtExpression(val string: Expression, val index: Expression) extends Expression {
  def tpe:ArgumentType = CharType()
}

// Access field access for the primary class
// TODO: OLD and deal with later
class InputExpression(val variableName:String) extends Expression {
  def tpe:ArgumentType = ???
}

class EqualExpression(val left: Expression, val right: Expression, val argType:ArgumentType = org.combinators.models.IntegerType()) extends Expression with BooleanExpression
class OrExpression(val left: Expression, val right: Expression) extends Expression with BooleanExpression
class AndExpression(val left: Expression, val right: Expression) extends Expression with BooleanExpression
class LessThanExpression(val left: Expression, val right:Expression) extends Expression with BooleanExpression
class LessThanOrEqualExpression(val left: Expression, val right:Expression) extends Expression with BooleanExpression
class TernaryExpression(val condition: Expression & BooleanExpression, val trueBranch: Expression, val falseBranch: Expression) extends Expression {
  def tpe:ArgumentType = {
    val trueType = trueBranch.tpe
    val falseType = falseBranch.tpe
    assert(trueType == falseType, "Mismatched types in TernaryExpression")
    trueType
  }
}

class LiteralBoolean(val literal:Boolean) extends LiteralExpression with BooleanExpression

// Now includes the name of the int variable to iterate over
class ArgExpression(val whichArg: Int, val name:String, val argType:ArgumentType, val itArgName:String) extends Expression {
  def tpe:ArgumentType = argType
}

// companion objects: needed for pattern matching? Might no longer be needed...
object AdditionExpression {
  def apply(left:Expression, right:Expression) = new AdditionExpression(left, right)
}
