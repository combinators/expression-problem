We designed a Model class that was meant to provide all information for a DP problem.

```class Model(val problem:String,
               val bounds: List[ArgExpression],
               val cases: List[(Option[Expression], Expression)],
               val retrieveLabel: String = "take sub-solution")
```

It is best to explain this with a simple model that captures the Fibonacci domain

# Fibonacci

```
    val zero: LiteralInt = LiteralInt(0)
    val one: LiteralInt = LiteralInt(1)
    val two: LiteralInt = LiteralInt(2)

    val bound = List(ArgExpression(0, "n", IntegerType(), "i"))

    val i: IteratorExpression = IteratorExpression(0, "i")
    
    new Model("Fibonacci",
      bound,
      cases = List(
        (Some(i == zero),   zero),
        (Some(i == one),    one),
        (None,              SubproblemExpression(Seq(i - one)) + SubproblemExpression(Seq(i - two)) )
      )
    )
  }
```

The `bounds` represents the arguments to the problem -- in this case, Fibonacci(n). `n` is an argument of type integer, and it is the 0th argument
for this problem. The variable "i" is to be used with iterating over subproblems in `n`.

The `cases` represents an ordered list of pairs, where each pair has an optional guard that, if true, yields the corresponding expression.

Thus `Fibonacci(0) = 0` and `Fibonacci(1) = 1`. The interesting case is the recursive case, namely that `Fibonacci(i)` is computed by adding together 
the resulting subexpression `Fibonacci(i-1)` and `Fibonacci(i-2)`.

# Solutions with Model

* DecodeWays
* Fibonacci
* JumpTo
* UniquePaths
* CoinChange
* KnapSack
* MaxSubArray
* ThreeStrings LongestCommonSubsequence
* DistinctSubsequences
* LongestCommonSubsequence
* MinimumEditDistance
* NeedlemanWunschSequenceAlignment
* UncrossedLines

In theory, each of these should support both a bottom-up generation and a top-down generation, using the `BottomUpStrategy` and `TopDownStrategy` found
in the `dp.original.BottomUpStrategy` and `dp.original.TopDownStrategy` traits.  