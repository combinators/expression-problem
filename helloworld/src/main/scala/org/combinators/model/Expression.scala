package org.combinators.model

trait Expression

//Integer
class AdditionExpression(val left: Expression, val right: Expression) extends Expression
class SubtractionExpression(val left: Expression, val right: Expression) extends Expression
class MultiplicationExpression(val left: Expression, val right: Expression) extends Expression
class DivisionExpression(val left: Expression, val right: Expression) extends Expression
class SubproblemExpression(val args: Seq[Expression]) extends Expression
class MaxExpression(val m: Expression, val n: Expression) extends Expression

class FunctionExpression(val name:String, val args: Seq[Expression]) extends Expression

class LiteralInt(val literal: Int) extends Expression
class IteratorExpression(val iteratorNumber: Int) extends Expression

class StringLengthExpression(val string: Expression)extends Expression

//String
class LiteralString(val literal: String) extends Expression
class SubstringExpression(val string: Expression, val start: Expression, val end: Expression) extends Expression

//Character
class CharAtExpression(val string: Expression, val index: Expression) extends Expression


//General
class EqualExpression(val left: Expression, val right: Expression) extends Expression

class ArgExpression(val whichArg: Int) extends Expression

// companion objects: needed for pattern matching
object AdditionExpression {
  def apply(left:Expression, right:Expression) = new AdditionExpression(left, right)
}
