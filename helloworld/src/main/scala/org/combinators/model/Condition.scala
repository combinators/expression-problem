package org.combinators.model

class Condition(val left: Expression, val right: Expression) {}

class StringEqualCondition(override val left: Expression, override val right: Expression) extends Condition(left, right) {}
