The `EnhancedModel` adds additional capabilities to support generating various DP problems.

```
class EnhancedModel(val problem:String,
                    val input:Seq[ArgExpression],
                    val subproblemType:ArgumentType,        // Type of solution
                    val solutionType:ArgumentType,          // Type of return value
                    val solution:SubproblemInvocation,
                    val definition:Definition,
                    val answer:Definition,                  // all existing Expression should just use ExpressionDefinition(expr)
                    val mode:ProblemOrder = Canonical())
```

