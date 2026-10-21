package org.combinators.equals.ffi.inbetween     /*DI:LI:AI*/

import org.combinators.cogen.Command.Generator
import org.combinators.cogen.paradigm.AnyParadigm as AP
import org.combinators.ep.language.inbetween.any.AnyParadigm
import org.combinators.equals.ffi.BaseType as BT
import org.combinators.cogen.{Command, TypeRep, Understands}
import org.combinators.ep.language.inbetween.ffi.FFI

// add capability to register arbitrary types in project
case class AddBaseType[Type](tpeLookup: (tpe: TypeRep) => Option[Type]) extends Command {
  type Result = Unit
} 

trait BaseType {
  val _base: AnyParadigm {val ast: BaseTypeAST}

  trait BaseTypeIn[Ctxt] extends BT[Ctxt] with FFI[Ctxt] {

    override val base: _base.type = _base
    
  }
}

object BaseType {
  type WithBase[B <: AnyParadigm] = BaseType { val _base: B }

  def apply[AST <: BaseTypeAST, B <: AnyParadigm.WithAST[AST]](base: B): WithBase[base.type] = {
    class BTs(val _base: base.type = base) extends BaseType {}
    new BTs()
  }
}
