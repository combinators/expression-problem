package org.combinators.cogen

/** Marks any inheriting object as a model of a software test case. */
trait TestCase {
  val tags: Seq[Tag] = Seq.empty
}
