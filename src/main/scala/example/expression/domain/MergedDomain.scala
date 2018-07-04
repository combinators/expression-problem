package example.expression.domain

/**
  * Merge e3 with i2.
  *
  * Must have a common ancestor (other than emptyModel()), otherwise merge is not well-defined
  */
trait MergedDomain extends Domain with IndependentDomain {

  val m1:Model = e3.merge("m1", i2)
}
