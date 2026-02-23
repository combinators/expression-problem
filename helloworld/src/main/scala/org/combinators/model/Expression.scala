package org.combinators.model

// Nice use of overloading to simplify the expressions!!!
trait Expression {
  def +(other: Expression): Expression = new AdditionExpression(this,other)
  def -(other: Expression): Expression = new SubtractionExpression(this,other)
  def *(other: Expression): Expression = new MultiplicationExpression(this,other)
  def /(other: Expression): Expression = new DivisionExpression(this,other)

  def <(other: Expression): Expression with BooleanExpression = new LessThanExpression(this,other)
  def <=(other: Expression): Expression with BooleanExpression = new LessThanOrEqualExpression(this,other)

  // When using ==, must assume it is IntegerType: Dangerous?? todo: allow for other types(?)
  def ==(other: Expression): Expression with BooleanExpression = new EqualExpression(this,other, new IntegerType())
  def ||(other: Expression): Expression with BooleanExpression = new OrExpression(this,other)
  def &&(other: Expression): Expression with BooleanExpression = new AndExpression(this,other)

  def apply(other: Expression): Expression = new ArrayElementExpression(this, other)
}

// tagging an expression as returning a Boolean value, which means it can be used in IfElseExpression
trait BooleanExpression


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
case class SubproblemInvocation(
       order:Seq[String],
       helpers:Map[String,HelperExpression] = Map.empty,     // known variables that are used in the problem expansion without being iterated over or called
       returnType: ArgumentType = IntegerType(),
       mappers: Map[String, Expression] = Map.empty)         // variables that map to new coordinates into dp[] space and are added to bottom up

class MaxExpression(val left: Expression, val right: Expression) extends Expression
class MinExpression(val left: Expression, val right: Expression) extends Expression
class ArrayElementExpression(val array: Expression, val index: Expression) extends Expression

class FunctionExpression(val name:String, val args: Seq[Expression]) extends Expression

class LiteralInt(val literal: Int) extends LiteralExpression
class IteratorExpression(val iteratorNumber: Int, val variable:String) extends Expression

// low and high are INCLUSIVE
case class HelperExpression(variable:String,
                            low:Expression,
                            in_range:Expression,
                            high:Expression) extends Expression

// when input problem has two integers, not easily translated as (row, column)
class LiteralPair(val val1:Int, val val2:Int) extends LiteralExpression
class LiteralTriple(val val1:Int, val val2:Int, val val3:Int) extends LiteralExpression

class LiteralArray(val literal:Array[Int]) extends LiteralExpression // assumes int array
class LiteralStringPair(val string1:String, val string2:String) extends LiteralExpression
class LiteralStringTriple(val string1:String, val string2:String, val string3:String) extends LiteralExpression

class LiteralArrayPair(val ar1:Array[Int], val ar2:Array[Int]) extends LiteralExpression

class Literal2DArrayIntPair(val ar1:Array[Int], val ar2:Array[Int], val value: Int) extends LiteralExpression

// For when a HelpExpression needs to refer to self
case class SelfExpression(val variableName:String) extends Expression

class StringLengthExpression(val string: Expression) extends Expression
class SubStringExpression(val string:Expression, val start:Expression, val exclusiveEnd:Expression) extends Expression
class ArrayLengthExpression(val array: Expression) extends Expression

//String
class LiteralString(val literal: String) extends LiteralExpression

//Character
class LiteralChar(val literal:Char) extends LiteralExpression
class CharAtExpression(val string: Expression, val index: Expression) extends Expression

// Access field access for the primary class
class InputExpression(val variableName:String) extends Expression

class EqualExpression(val left: Expression, val right: Expression, val tpe:ArgumentType = org.combinators.model.IntegerType()) extends Expression with BooleanExpression
class OrExpression(val left: Expression, val right: Expression) extends Expression with BooleanExpression
class AndExpression(val left: Expression, val right: Expression) extends Expression with BooleanExpression
class LessThanExpression(val left: Expression, val right:Expression) extends Expression with BooleanExpression
class LessThanOrEqualExpression(val left: Expression, val right:Expression) extends Expression with BooleanExpression
class TernaryExpression(val condition: Expression with BooleanExpression, val trueBranch: Expression, val falseBranch: Expression) extends Expression

class LiteralBoolean(val literal:Boolean) extends LiteralExpression

// Now includes the name of the int variable to iterate over
class ArgExpression(val whichArg: Int, val name:String, val argType:ArgumentType, val itArgName:String) extends Expression

// companion objects: needed for pattern matching? Might no longer be needed...
object AdditionExpression {
  def apply(left:Expression, right:Expression) = new AdditionExpression(left, right)
}
