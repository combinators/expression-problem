package org.combinators.equals.ffi.inbetween     /*DI:LI:AI*/

import org.combinators.cogen.Command.Generator
import org.combinators.cogen.paradigm.{AnyParadigm => AP}
import org.combinators.ep.language.inbetween.any.AnyParadigm
import org.combinators.equals.ffi.BaseType as BT
import org.combinators.cogen.{Command, TypeRep, Understands}

// add capability to register arbitrary types in project
case class AddBaseType[Type](tpeLookup: (tpe: TypeRep) => Option[Type]) extends Command {
  type Result = Unit
} 

trait BaseType[AST <: BaseTypeAST, B, Context](val base: AnyParadigm.WithAST[AST] & B) extends BT[Context] {

  import base.ast.baseTypeOpsFactory
  import base.ast.any

  def registerTypeLookup(context: any.Project, lookup: (tpe: TypeRep) => Option[any.Type]) : any.Project
  
  trait BaseTypeCapabilities {
    implicit val canAddContextTypeLookup: Understands[any.Project, AddBaseType[any.Type]] = new Understands[any.Project, AddBaseType[any.Type]] {
      override def perform(context: any.Project, command: AddBaseType[any.Type]): (any.Project, Unit) = {
        val newContext = registerTypeLookup(context, command.tpeLookup)
        (newContext, ())
      }
    }

    def addContextTypeLookup(tpeLookup: (tpe: TypeRep) => Option[any.Type]): Generator[any.Project, Unit] =
      AP.capability(AddBaseType[any.Type](tpeLookup))
  }

  val baseTypeCapabilities: BaseTypeCapabilities = new BaseTypeCapabilities {}

  // once enabled, all types are registered 
  override def enable(): Generator[any.Project, Unit] = {
    for {
      _ <- baseTypeCapabilities.addContextTypeLookup({
        case BT.AnyTpe => Some(baseTypeOpsFactory.baseType())
        case BT.CompositeTpe(arg) => Some(baseTypeOpsFactory.compositeType(arg))
        case _ => None
      })
    } yield ()
  }
}

object BaseType {

  type WithBase[AST <: BaseTypeAST, B <: AnyParadigm.WithAST[AST], Context] = BaseType[AST, B, Context] {}

  def apply[AST <: BaseTypeAST, B <: AnyParadigm.WithAST[AST], Context](
          _base: B)(
          _addContextTypeLookup: (context: _base.ast.any.Project, lookup: (tpe: TypeRep) => Option[_base.ast.any.Type]) => _base.ast.any.Project
  ): WithBase[AST, _base.type, Context] = new BaseType[AST, _base.type, Context](_base) {
    
    override def registerTypeLookup(context: _base.ast.any.Project, lookup: (tpe: TypeRep) => Option[_base.ast.any.Type]): _base.ast.any.Project =
      _addContextTypeLookup(context, lookup)
  }
}
