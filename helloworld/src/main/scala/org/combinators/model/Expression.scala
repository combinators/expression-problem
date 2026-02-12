package org.combinators.model

// Nice use of overloading to simplify the expressions!!!
trait Expression {
  def +(other: Expression): Expression = new AdditionExpression(this,other)
  def -(other: Expression): Expression = new SubtractionExpression(this,other)
  def *(other: Expression): Expression = new MultiplicationExpression(this,other)
  def /(other: Expression): Expression = new DivisionExpression(this,other)
  def <(other: Expression): Expression = new LessThanExpression(this,other)
  def <=(other: Expression): Expression = new LessThanOrEqualExpression(this,other)

  // When using ==, must assume it is IntegerType: Dangerous?? todo: allow for other types(?)
  def ==(other: Expression): Expression = new EqualExpression(this,other, new IntegerType())
  def ||(other: Expression): Expression = new OrExpression(this,other)
  def &&(other: Expression): Expression = new AndExpression(this,other)

  def apply(other: Expression): Expression = new ArrayElementExpression(this, other)
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

// If helper is defined but NOT part of the parameters during invocation, then it must be passed in as helpers
case class SubproblemInvocation(parameters: Map[String,(Expression,HelperExpression)], order:Seq[String], helpers:Map[String,HelperExpression] = Map.empty, returnType: ArgumentType = IntegerType())

class MathMinimumExpression(val args: Seq[Expression]) extends Expression
class MaxExpression(val left: Expression, val right: Expression) extends Expression
class MinExpression(val left: Expression, val right: Expression) extends Expression
class ArrayElementExpression(val array: Expression, val index: Expression) extends Expression

class FunctionExpression(val name:String, val args: Seq[Expression]) extends Expression

class LiteralInt(val literal: Int) extends LiteralExpression
class IteratorExpression(val iteratorNumber: Int, val variable:String) extends Expression

// low and high are INCLUSIVE
case class HelperExpression(variable:String, low:Expression, in_range:Expression) extends Expression

// when input problem has two integers, not easily translated as (row, column)
class LiteralPair(val val1:Int, val val2:Int) extends LiteralExpression

class LiteralArray(val ar:Array[Int]) extends LiteralExpression
class LiteralStringPair(val string1:String, val string2:String) extends LiteralExpression
class LiteralStringTriple(val string1:String, val string2:String, val string3:String) extends LiteralExpression

class LiteralTwoArrays(val ar1:Array[Int], val ar2:Array[Int]) extends LiteralExpression

// For when a HelpExpression needs to refer to self
case class SelfExpression(val variableName:String) extends Expression

class StringLengthExpression(val string: Expression) extends Expression
class ArrayLengthExpression(val array: Expression) extends Expression

//String
class LiteralString(val literal: String) extends LiteralExpression
class SubstringExpression(val string: Expression, val start: Expression, val end: Expression) extends Expression

//Character
class LiteralChar(val literal:Char) extends LiteralExpression
class CharAtExpression(val string: Expression, val index: Expression) extends Expression

// Access field access for the primary class
class InputExpression(val variableName:String) extends Expression

class EqualExpression(val left: Expression, val right: Expression, val tpe:ArgumentType = new org.combinators.model.IntegerType()) extends Expression
class OrExpression(val left: Expression, val right: Expression) extends Expression
class AndExpression(val left: Expression, val right: Expression) extends Expression
class LessThanExpression(val left: Expression, val right:Expression) extends Expression
class LessThanOrEqualExpression(val left: Expression, val right:Expression) extends Expression
class TernaryExpression(val condition: Expression, val trueBranch: Expression, val falseBranch: Expression) extends Expression

class LiteralBoolean(val literal:Boolean) extends LiteralExpression

// Now includes the name of the int variable to iterate over
class ArgExpression(val whichArg: Int, val name:String, val argType:ArgumentType, val itArgName:String) extends Expression

// companion objects: needed for pattern matching
object AdditionExpression {
  def apply(left:Expression, right:Expression) = new AdditionExpression(left, right)
}
