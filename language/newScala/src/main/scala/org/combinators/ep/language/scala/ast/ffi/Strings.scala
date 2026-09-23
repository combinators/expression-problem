package org.combinators.ep.language.scala.ast.ffi

import org.combinators.cogen.Command.Generator
import org.combinators.cogen.{Command, TypeRep}
import org.combinators.ep.language.inbetween.{ContextRegistry, ffi}
import org.combinators.ep.language.inbetween.any.AnyParadigm
import org.combinators.ep.language.scala.ast.BaseAST
import org.combinators.ep.language.inbetween.ffi.Strings as Str

import scala.reflect.{ClassTag, classTag}

trait Strings extends Str {
  override val _base: AnyParadigm {val ast: StringAST & BaseAST }
  val methodRegistry: ContextRegistry[_base.type, _base.ast.any.Method]
  val constructorRegistry: ContextRegistry[_base.type, _base.ast.oo.Constructor]
  val classRegistry: ContextRegistry[_base.type, _base.ast.oo.Class]
  val nameProvider: _base.ast.nameProvider.ScalaNameProvider = _base.ast.nameProviderFactory.scalaNameProvider

  trait ScalaStringsIn[Ctxt] extends super.StringsIn[Ctxt] {
    override val tpeLookup: TypeRep => Option[Generator[Ctxt, _base.syntax.Type]] = {
      case TypeRep.String =>
        Some(Command.lift(_base.ast.ooFactory.classReferenceType(nameProvider.mangle("String"))))
      case _ => None
    }
    override val reifylookup: (tpeRep: TypeRep) => tpeRep.HostType => Option[Generator[Ctxt, _base.syntax.Expression]] = {
      case TypeRep.String => s =>
        Some(Command.lift(_base.ast.scalaBaseFactory.reifiedScalaValue[String](TypeRep.String, s.asInstanceOf[String], None)))
      case _ => b => None
    }
  }

  val stringsInMethods: ScalaStringsIn[_base.ast.any.Method] = {
    class Strs(
      override val registry: ContextRegistry[_base.type, _base.ast.any.Method] = methodRegistry
    ) extends ScalaStringsIn[_base.ast.any.Method] {}
    new Strs()
  }
  val stringsInConstructors: ScalaStringsIn[_base.ast.oo.Constructor] = {
    class Strs(
      override val registry: ContextRegistry[_base.type, _base.ast.oo.Constructor] = constructorRegistry
    ) extends ScalaStringsIn[_base.ast.oo.Constructor] {}
    new Strs()
  }
  val stringsInClasses: ScalaStringsIn[_base.ast.oo.Class] = {
    class Strs(
      override val registry: ContextRegistry[_base.type, _base.ast.oo.Class] = classRegistry
    ) extends ScalaStringsIn[_base.ast.oo.Class] {}
    new Strs()
  }
}

object Strings {
  type WithBase[B <: AnyParadigm] = Strings { val _base: B }
  def apply[AST <: StringAST & BaseAST, B <: AnyParadigm.WithAST[AST]](
    base: B,
    methodRegistry: ContextRegistry[base.type, base.ast.any.Method],
    constructorRegistry: ContextRegistry[base.type, base.ast.oo.Constructor],
    classRegistry: ContextRegistry[base.type, base.ast.oo.Class],
  ): Strings.WithBase[base.type] = {
    class Strs(
      override val _base: base.type,
      override val methodRegistry: ContextRegistry[_base.type, _base.ast.any.Method],
      override val constructorRegistry: ContextRegistry[_base.type, _base.ast.oo.Constructor],
      override val classRegistry: ContextRegistry[_base.type, _base.ast.oo.Class],
    ) extends Strings
    new Strs(base, methodRegistry, constructorRegistry, classRegistry) {}
  }
}
