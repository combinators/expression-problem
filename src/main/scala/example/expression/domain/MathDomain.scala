package example.expression.domain  /*DD:LI:AI*/

/**
  * Mathematical Expressions domain as an instance of EP.
  */
trait MathDomain extends BaseDomain with ModelDomain {

  case object Exp extends TypeRep {
    override def name:String = "Exp"
  }
  type BaseTypeRep = Exp.type
  val baseTypeRep:BaseTypeRep = Exp
}

/** Companion object to represent domain. */
object companionMathDomain extends MathDomain
