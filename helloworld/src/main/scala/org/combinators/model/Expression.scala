package org.combinators.model

trait Expression {
  def +(other: Expression): Expression = new AdditionExpression(this,other)
  def -(other: Expression): Expression = new SubtractionExpression(this,other)
  def *(other: Expression): Expression = new MultiplicationExpression(this,other)
  def /(other: Expression): Expression = new DivisionExpression(this,other)
  def ==(other: Expression): Expression = new EqualExpression(this,other)
}

//Integer
class AdditionExpression(val left: Expression, val right: Expression) extends Expression
class SubtractionExpression(val left: Expression, val right: Expression) extends Expression
class MultiplicationExpression(val left: Expression, val right: Expression) extends Expression
class DivisionExpression(val left: Expression, val right: Expression) extends Expression
class SubproblemExpression(val args: Seq[Expression]) extends Expression
class MaxExpression(val m: Expression, val n: Expression) extends Expression
class MinExpression(val m: Expression, val n: Expression) extends Expression
class ArrayElementExpression(val array: Expression, val index: Expression) extends Expression

class FunctionExpression(val name:String, val args: Seq[Expression]) extends Expression

class LiteralInt(val literal: Int) extends Expression
class IteratorExpression(val iteratorNumber: Int, val variable:String) extends Expression

class StringLengthExpression(val string: Expression)extends Expression
class ArrayLengthExpression(val array: Expression) extends Expression

//String
class LiteralString(val literal: String) extends Expression
class SubstringExpression(val string: Expression, val start: Expression, val end: Expression) extends Expression

//Character
class LiteralChar(val char:Char) extends Expression
class CharAtExpression(val string: Expression, val index: Expression) extends Expression


//General
class InputExpression(val variableName:String) extends Expression
class EqualExpression(val left: Expression, val right: Expression) extends Expression
class orExpression(val left: Expression, val right: Expression) extends Expression
class lessThanExpression(val left: Expression, val right:Expression) extends Expression

class ArgExpression(val whichArg: Int, val name:String, val argType:ArgumentType) extends Expression

// companion objects: needed for pattern matching
object AdditionExpression {
  def apply(left:Expression, right:Expression) = new AdditionExpression(left, right)
}
