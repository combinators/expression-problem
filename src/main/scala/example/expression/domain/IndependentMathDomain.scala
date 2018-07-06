package example.expression.domain

/**
  * Develop two new independent extensions to e1
  */
trait IndependentMathDomain extends MathDomain {

  object independent {
    val height:String = "height"
  }

  // i1 extension
  // ------------
  case object Inv extends Unary("Inv")
  val i1 = Model("i1", Seq(Inv), Seq.empty, last=m1)

  // i2 extension
  // ------------
  case object Integer extends TypeRep
  case object Height extends Operation(independent.height, Some(Integer), Seq((independent.height, Integer)))
  val i2 = Model("i2", Seq.empty, Seq(Height), last=i1)
}
