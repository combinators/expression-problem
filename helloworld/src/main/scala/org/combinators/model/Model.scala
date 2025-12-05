package org.combinators.model

class Model(val inputs: List[Expression], val iterators: List[IteratorExpression], val cases: List[(Option[Expression], Expression)])

// todo: figure out how to represent transitions (subproblems)

// test example in bottomUp/twoSequences/longestcommonsubsequence/LongestCommonSubsequenceObjectOrientedProvider.scala