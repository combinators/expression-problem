package org.combinators.model

class Model(val problem:String, val bounds: List[Expression], val cases: List[(Option[Expression], Expression)])

// test example in bottomUp/twoSequences/longestcommonsubsequence/LongestCommonSubsequenceObjectOrientedProvider.scala