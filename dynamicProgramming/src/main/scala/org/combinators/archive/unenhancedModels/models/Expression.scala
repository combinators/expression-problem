package org.combinators.archive.unenhancedModels.models

import org.combinators.models.{ArgumentType, LiteralExpression}

/** These are archived Literal expressions, only to be used by archive, older vode. */
case class StringPairType() extends ArgumentType
case class LiteralStringPair(string1:String, string2:String) extends LiteralExpression {
  def tpe:ArgumentType = StringPairType()
}

case class StringTripleType() extends ArgumentType
case class LiteralStringTriple(string1:String, string2:String, string3:String) extends LiteralExpression {
  def tpe:ArgumentType = StringTripleType()
}

case class LiteralPairType() extends ArgumentType
case class LiteralPair(val1:Int, val2:Int) extends LiteralExpression {
  def tpe:ArgumentType = LiteralPairType()
}

case class LiteralTripleType() extends ArgumentType
case class LiteralTriple(val1: Int, val2: Int, val3: Int) extends LiteralExpression {
  def tpe: ArgumentType = LiteralTripleType()
}

case class IntegerArrayPair() extends ArgumentType
case class LiteralArrayPair(ar1: Array[Int], ar2: Array[Int]) extends LiteralExpression {
  def tpe: ArgumentType = IntegerArrayPair()
}
