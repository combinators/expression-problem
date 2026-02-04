package org.combinators.model

// Nice use of overloading to simplify the expressions!!!
trait Expression {
  def +(other: Expression): Expression = new AdditionExpression(this,other)
  def -(other: Expression): Expression = new SubtractionExpression(this,other)
  def *(other: Expression): Expression = new MultiplicationExpression(this,other)
  def /(other: Expression): Expression = new DivisionExpression(this,other)

  // When using ==, must assume it is IntegerType: Dangerous??
  def ==(other: Expression): Expression = new EqualExpression(this,other, new IntegerType())
}

// necessary for defining literals that form the input or possible output
trait LiteralExpression extends Expression
class UnitExpression extends LiteralExpression    // VOID

//Integer
class AdditionExpression(val left: Expression, val right: Expression) extends Expression
class SubtractionExpression(val left: Expression, val right: Expression) extends Expression
class MultiplicationExpression(val left: Expression, val right: Expression) extends Expression
class DivisionExpression(val left: Expression, val right: Expression) extends Expression
class SubproblemExpression(val args: Seq[Expression]) extends Expression
class MaxExpression(val left: Expression, val right: Expression) extends Expression
class MinExpression(val m: Expression, val n: Expression) extends Expression
class ArrayElementExpression(val array: Expression, val index: Expression) extends Expression

class FunctionExpression(val name:String, val args: Seq[Expression]) extends Expression

class LiteralInt(val literal: Int) extends LiteralExpression
class IteratorExpression(val iteratorNumber: Int, val variable:String) extends Expression

// when input problem has two integers, not easily translated as (row, column)
class LiteralPair(val val1:Int, val val2:Int) extends LiteralExpression

class LiteralArray(val ar:Array[Int]) extends LiteralExpression
class LiteralStringPair(val string1:String, val string2:String) extends LiteralExpression

class StringLengthExpression(val string: Expression)extends Expression
class ArrayLengthExpression(val array: Expression) extends Expression

//String
class LiteralString(val literal: String) extends LiteralExpression
class SubstringExpression(val string: Expression, val start: Expression, val end: Expression) extends Expression

//Character
class LiteralChar(val char:Char) extends LiteralExpression
class CharAtExpression(val string: Expression, val index: Expression) extends Expression


//General
class InputExpression(val variableName:String) extends Expression
class EqualExpression(val left: Expression, val right: Expression, val tpe:ArgumentType = new org.combinators.model.IntegerType()) extends Expression
class orExpression(val left: Expression, val right: Expression) extends Expression
class lessThanExpression(val left: Expression, val right:Expression) extends Expression

// Now includes the name of the int variable to iterate over
class ArgExpression(val whichArg: Int, val name:String, val argType:ArgumentType, val itArgName:String) extends Expression

// companion objects: needed for pattern matching
object AdditionExpression {
  def apply(left:Expression, right:Expression) = new AdditionExpression(left, right)
}
