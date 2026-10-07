package org.combinators.ep.language.inbetween.ffi    /*DI:LI:AI*/
import org.combinators.cogen.paradigm.ffi.{ Units as Uns}
import org.combinators.ep.language.inbetween.any.AnyParadigm
import org.combinators.ep.language.inbetween.any.AnyParadigm.WithAST

trait Units {
  val _base: AnyParadigm { val ast: UnitAST }
  trait UnitsIn[Ctxt] extends Uns[Ctxt] with FFI[Ctxt] {
    override val base: _base.type = _base

    val unitCapabilities: UnitCapabilities =
      new UnitCapabilities {
        
      }
  }
}

object Units {
  type WithBase[B <: AnyParadigm] = Units { val _base: B }

  def apply[AST <: UnitAST, B <: AnyParadigm.WithAST[AST]](base: B): WithBase[base.type] = {
    class Uns(override val _base: base.type = base) extends Units {}
    new Uns()
  }
}
