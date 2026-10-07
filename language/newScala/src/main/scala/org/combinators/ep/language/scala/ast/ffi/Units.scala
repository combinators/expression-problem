package org.combinators.ep.language.scala.ast.ffi

import org.combinators.cogen.Command.Generator
import org.combinators.cogen.{Command, TypeRep}
import org.combinators.ep.language.inbetween.ContextRegistry
import org.combinators.ep.language.inbetween.any.AnyParadigm
import org.combinators.ep.language.scala.ast.BaseAST
import org.combinators.ep.language.inbetween.ffi.Units as Uns

trait Units extends Uns {
  override val _base: AnyParadigm {val ast: UnitAST & BaseAST }
  val methodRegistry: ContextRegistry[_base.type, _base.ast.any.Method]
  val constructorRegistry: ContextRegistry[_base.type, _base.ast.oo.Constructor]
  val classRegistry: ContextRegistry[_base.type, _base.ast.oo.Class]

  val nameProvider: _base.ast.nameProvider.ScalaNameProvider = _base.ast.nameProviderFactory.scalaNameProvider
  
  trait ScalaUnitsIn[Ctxt] extends super.UnitsIn[Ctxt] {
    override val tpeLookup: TypeRep => Option[Generator[Ctxt, _base.syntax.Type]] = {
      case TypeRep.Unit =>
        val name = "Unit"
        Some(Command.lift(_base.ast.ooFactory.classReferenceType(_base.ast.scalaBaseFactory.name(name, name))))
      case _ => None
    }
    override val reifylookup: (tpeRep: TypeRep) => tpeRep.HostType => Option[Generator[Ctxt, _base.syntax.Expression]] = {
      case TypeRep.Unit => u => Some(Command.lift(
          _base.ast.unitOpsFactory.unitExp()
        ))
      
      case _ => u => None
    }
  }
  
  val unitsInMethods: ScalaUnitsIn[_base.ast.any.Method] = {
    class Uns(
      override val registry: ContextRegistry[_base.type, _base.ast.any.Method] = methodRegistry
    ) extends ScalaUnitsIn[_base.ast.any.Method] {}
    new Uns()
  }
  val unitsInConstructors: ScalaUnitsIn[_base.ast.oo.Constructor] = {
    class Uns(
      override val registry: ContextRegistry[_base.type, _base.ast.oo.Constructor] = constructorRegistry
    ) extends ScalaUnitsIn[_base.ast.oo.Constructor] {}
    new Uns()
  }
  val unitsInClasses: ScalaUnitsIn[_base.ast.oo.Class] = {
    class Uns(
      override val registry: ContextRegistry[_base.type, _base.ast.oo.Class] = classRegistry
    ) extends ScalaUnitsIn[_base.ast.oo.Class] {}
    new Uns()
  }
}

object Units {
  type WithBase[B <: AnyParadigm] = Units { val _base: B }
  def apply[AST <: UnitAST & BaseAST, B <: AnyParadigm.WithAST[AST]](
    base: B,
    methodRegistry: ContextRegistry[base.type, base.ast.any.Method],
    constructorRegistry: ContextRegistry[base.type, base.ast.oo.Constructor],
    classRegistry: ContextRegistry[base.type, base.ast.oo.Class],
  ): Units.WithBase[base.type] = {
    class Uns(
      override val _base: base.type,
      override val methodRegistry: ContextRegistry[_base.type, _base.ast.any.Method],
      override val constructorRegistry: ContextRegistry[_base.type, _base.ast.oo.Constructor],
      override val classRegistry: ContextRegistry[_base.type, _base.ast.oo.Class],
    ) extends Units
    new Uns(base, methodRegistry, constructorRegistry, classRegistry) {}
  }
}
