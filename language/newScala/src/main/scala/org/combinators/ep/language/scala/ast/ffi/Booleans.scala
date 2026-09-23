package org.combinators.ep.language.scala.ast.ffi

import org.combinators.cogen.Command.Generator
import org.combinators.cogen.{Command, TypeRep}
import org.combinators.ep.language.inbetween.{ContextRegistry, ffi}
import org.combinators.ep.language.inbetween.any.AnyParadigm
import org.combinators.ep.language.scala.ast.BaseAST
import org.combinators.ep.language.inbetween.ffi.Booleans as Bools

import scala.reflect.{ClassTag, classTag}

trait Booleans extends Bools {
  override val _base: AnyParadigm {val ast: BooleanAST & BaseAST }
  val methodRegistry: ContextRegistry[_base.type, _base.ast.any.Method]
  val constructorRegistry: ContextRegistry[_base.type, _base.ast.oo.Constructor]
  val classRegistry: ContextRegistry[_base.type, _base.ast.oo.Class]

  val nameProvider: _base.ast.nameProvider.ScalaNameProvider = _base.ast.nameProviderFactory.scalaNameProvider
  
  trait ScalaBooleansIn[Ctxt] extends super.BooleansIn[Ctxt] {
    override val tpeLookup: TypeRep => Option[Generator[Ctxt, _base.syntax.Type]] = {
      case TypeRep.Boolean =>
        Some(Command.lift(_base.ast.ooFactory.classReferenceType(nameProvider.mangle("Boolean"))))
      case _ => None
    }
    override val reifylookup: (tpeRep: TypeRep) => tpeRep.HostType => Option[Generator[Ctxt, _base.syntax.Expression]] = {
      case TypeRep.Boolean => b => Some(Command.lift(
        if (b.asInstanceOf[scala.Boolean]) {
          _base.ast.booleanOpsFactory.trueExp()
        } else {
          _base.ast.booleanOpsFactory.falseExp()
        }))
      case _ => b => None
    }
  }
  
  val booleansInMethods: ScalaBooleansIn[_base.ast.any.Method] = {
    class Bls(
      override val registry: ContextRegistry[_base.type, _base.ast.any.Method] = methodRegistry
    ) extends ScalaBooleansIn[_base.ast.any.Method] {}
    new Bls()
  }
  val booleansInConstructors: ScalaBooleansIn[_base.ast.oo.Constructor] = {
    class Bls(
      override val registry: ContextRegistry[_base.type, _base.ast.oo.Constructor] = constructorRegistry
    ) extends ScalaBooleansIn[_base.ast.oo.Constructor] {}
    new Bls()
  }
  val booleansInClasses: ScalaBooleansIn[_base.ast.oo.Class] = {
    class Bls(
      override val registry: ContextRegistry[_base.type, _base.ast.oo.Class] = classRegistry
    ) extends ScalaBooleansIn[_base.ast.oo.Class] {}
    new Bls()
  }
}

object Booleans {
  type WithBase[B <: AnyParadigm] = Booleans { val _base: B }
  def apply[AST <: BooleanAST & BaseAST, B <: AnyParadigm.WithAST[AST]](
    base: B,
    methodRegistry: ContextRegistry[base.type, base.ast.any.Method],
    constructorRegistry: ContextRegistry[base.type, base.ast.oo.Constructor],
    classRegistry: ContextRegistry[base.type, base.ast.oo.Class],
  ): Booleans.WithBase[base.type] = {
    class Bls(
      override val _base: base.type,
      override val methodRegistry: ContextRegistry[_base.type, _base.ast.any.Method],
      override val constructorRegistry: ContextRegistry[_base.type, _base.ast.oo.Constructor],
      override val classRegistry: ContextRegistry[_base.type, _base.ast.oo.Class],
    ) extends Booleans
    new Bls(base, methodRegistry, constructorRegistry, classRegistry) {}
  }
}
