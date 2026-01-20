package org.combinators.model

trait Expression

class IntegerExpression extends Expression
class LiteralInt(val literal: Int) extends IntegerExpression
class IteratorExpression(var iteratorNumber: Int) extends IntegerExpression

class StringExpression extends Expression
class LiteralString(val literal: String) extends StringExpression
class SubstringExpression(var string: StringExpression, var start: Expression, var end: Expression) extends StringExpression

class CharacterExpression extends Expression
class CharAtExpression(var string: StringExpression, var index: Expression) extends CharacterExpression

class BooleanExpression extends Expression
class EqualExpression(var left: Expression, var right: Expression) extends BooleanExpression

abstract class SubproblemExpression(var args: Expression*) extends Expression