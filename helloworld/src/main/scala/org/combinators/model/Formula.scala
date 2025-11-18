package org.combinators.model

abstract class Formula() {}

class ArrayAccessFormula(val targetArray: String, val targetIndex: String) extends Formula {}

class ConstantFormula(val number: String) extends Formula
