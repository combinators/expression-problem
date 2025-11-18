package org.combinators.model

class Expression(val targetArray: String, val targetIndex: String) {}

abstract class MathematicalExpression() extends Formula {}

class AdditionExpression(val left: Formula, right: Formula) extends MathematicalExpression {}