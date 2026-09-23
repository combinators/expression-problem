package org.combinators.ep.language.scala.ast.ffi

import org.combinators.cogen.Command.Generator
import org.combinators.cogen.{Command, TypeRep}
import org.combinators.ep.language.inbetween.{ContextRegistry, ffi}
import org.combinators.ep.language.inbetween.any.AnyParadigm
import org.combinators.ep.language.scala.ast.BaseAST
import org.combinators.ep.language.inbetween.ffi.Console as Cons

import scala.reflect.{ClassTag, classTag}

trait Console extends Cons {
  override val _base: AnyParadigm { val ast: ConsoleAST & BaseAST }
  val methodRegistry: ContextRegistry[_base.type, _base.ast.any.Method]
  val constructorRegistry: ContextRegistry[_base.type, _base.ast.oo.Constructor]
  val classRegistry: ContextRegistry[_base.type, _base.ast.oo.Class]

  trait ScalaConsoleIn[Ctxt] extends super.ConsoleIn[Ctxt] {
    override val tpeLookup: TypeRep => Option[Generator[Ctxt, _base.syntax.Type]] =
      _ => None
    override val reifylookup: (tpeRep: TypeRep) => tpeRep.HostType => Option[Generator[Ctxt, _base.syntax.Expression]] =
      _ => _ => None
  }

  val consoleInMethods: ScalaConsoleIn[_base.ast.any.Method] = {
    class ConsoleInMethods(
      override val registry: ContextRegistry[_base.type, _base.ast.any.Method] = methodRegistry
    ) extends ScalaConsoleIn[_base.ast.any.Method]
    new ConsoleInMethods() {}
  }
  val consoleInConstructors: ScalaConsoleIn[_base.ast.oo.Constructor] = {
    class ConsoleInConstructors(
      override val registry: ContextRegistry[_base.type, _base.ast.oo.Constructor] = constructorRegistry
    ) extends ScalaConsoleIn[_base.ast.oo.Constructor]
    new ConsoleInConstructors() {}
  }

  val consoleInClasses: ScalaConsoleIn[_base.ast.oo.Class] = {
    class ConsoleInClasses(
      override val registry: ContextRegistry[_base.type, _base.ast.oo.Class] = classRegistry
    ) extends ScalaConsoleIn[_base.ast.oo.Class]
    new ConsoleInClasses() {}
  }
}

object Console {
  type WithBase[B <: AnyParadigm] = Console { val _base: B }
  def apply[AST <: ConsoleAST & BaseAST, B <: AnyParadigm.WithAST[AST]](
    base: B,
    methodRegistry: ContextRegistry[base.type, base.ast.any.Method],
    constructorRegistry: ContextRegistry[base.type, base.ast.oo.Constructor],
    classRegistry: ContextRegistry[base.type, base.ast.oo.Class],
  ): Console.WithBase[base.type] = {
    class Consl(
      override val _base: base.type,
      override val methodRegistry: ContextRegistry[_base.type, _base.ast.any.Method],
      override val constructorRegistry: ContextRegistry[_base.type, _base.ast.oo.Constructor],
      override val classRegistry: ContextRegistry[_base.type, _base.ast.oo.Class],
    ) extends Console {}
    new Consl(base, methodRegistry, constructorRegistry, classRegistry)
  }
}
