package example.expression.domain

/**
  * Merge e3 with i2.
  *
  * Must have a common ancestor (other than emptyModel()), otherwise merge is not well-defined
  */
trait MergedMathDomain extends MathDomain with IndependentMathDomain {

  val c1:Model = m3.merge("c1", i2)
}
