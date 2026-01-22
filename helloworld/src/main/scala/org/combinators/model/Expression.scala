package org.combinators.model

trait Expression

//Integer
class AdditionExpression(val left: Expression, val right: Expression) extends Expression
class SubtractionExpression(val left: Expression, val right: Expression) extends Expression
class MultiplicationExpression(val left: Expression, val right: Expression) extends Expression
class DivisionExpression(val left: Expression, val right: Expression) extends Expression
class SubproblemExpression(var args: Seq[Expression]) extends Expression
class MaxExpression(var m: Expression, var n: Expression) extends Expression

class LiteralInt(val literal: Int) extends Expression
class IteratorExpression(var iteratorNumber: Int) extends Expression

class StringLengthExpression(var string: Expression)extends Expression

//String
class LiteralString(val literal: String) extends Expression
class SubstringExpression(var string: Expression, var start: Expression, var end: Expression) extends Expression

//Character
class CharAtExpression(var string: Expression, var index: Expression) extends Expression


//General
class EqualExpression(var left: Expression, var right: Expression) extends Expression

class ArgExpression(whichArg: Int) extends Expression