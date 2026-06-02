package org.combinators.cogen     /*DI:LI:AI*/

/** Marks any inheriting object as a model of a software test case. */
trait TestCase {
  val tags: Seq[Tag] = Seq.empty
}
