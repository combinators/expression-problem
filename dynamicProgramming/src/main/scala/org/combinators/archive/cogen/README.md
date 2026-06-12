This `cogen` package contains the earliest attempts to use CoGen to generate dynamic programming (DP) solutions in Java.

Starting from available Java implementations of numerous DP problems, in the initial stage of this project the goal was to be able
to replicate these code using straight CoGen code. Along the way, one hoped it would be possible to find opportunities to reuse 
CoGen methods to be able to normalize the entire process.

# Bottom Up

* Pascal's Triangle
* One Sequence
  - DecodeWays
  - Fibonacci
  - MaxSubArray
  - Tribonacci
* Two Sequences
  - LongestCommonSubSequence
  - UncrossedLines

# Top Down

* Pascal's Triangle
* One Sequence
  - Delete And Earn
  - House Robber
  - JumpTo
  - Tribonacci

# Summary

While various DP problems can be generated through straight CoGen code, this approach is untenable and ultimately did not really result
in reusable methods for these problems.

