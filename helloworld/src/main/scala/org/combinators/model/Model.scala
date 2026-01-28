package org.combinators.model

trait ArgumentType { }

// problem instance types go here
class IntegerType extends ArgumentType
class StringType extends ArgumentType
class DoubleType extends ArgumentType

// possibly choose to make this Generic but that seems like overkill
class IntegerArrayType extends ArgumentType

class Argument (val argName:String, val argType:ArgumentType)

class Model(val problem:String, val bounds: List[ArgExpression], val cases: List[(Option[Expression], Expression)])

// test example in bottomUp/twoSequences/longestcommonsubsequence/LongestCommonSubsequenceObjectOrientedProvider.scala