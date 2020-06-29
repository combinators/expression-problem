package org.combinators.ep.language.scala   /*DI:LD:AI*/

import cats.{Apply => _}
import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.generator.Command
import org.combinators.ep.generator.paradigm.ffi.Trees
import org.combinators.ep.generator.paradigm.{Generics, ParametricPolymorphism}
import org.combinators.ep.language.scala.paradigm._
import org.combinators.ep.language.scala.paradigm.ffi._

import scala.meta._

/**
  * Scala-specific.
  *
  * These paradigm-specific traits are conceptually different from each other
  */
sealed class CodeGenerator(config: Config) { cc =>
  val paradigm: AnyParadigm = AnyParadigm(config)
  val functional: Functional[paradigm.type] = Functional(paradigm)
  val functionalInMethod: control.Functional[paradigm.MethodBodyContext, paradigm.type] = control.Functional.inMethodContext(paradigm)

  /*val ooParadigm: ObjectOriented[paradigm.type] = ObjectOriented(paradigm)
  val imperativeInMethod: Imperative[MethodBodyCtxt, paradigm.type] = Imperative.inMethodContext(paradigm)
  val imperativeInConstructor: Imperative[CtorCtxt, paradigm.type] = Imperative.inConstructorContext(paradigm)
  val parametricPolymorphism: ParametricPolymorphism[paradigm.type] = ParametricPolymorphism(paradigm)
  val generics: Generics.Aux[paradigm.type, ooParadigm.type, parametricPolymorphism.type] = Generics(paradigm)(ooParadigm, parametricPolymorphism)*/

  val booleansInMethod = new Booleans[MethodBodyCtxt, paradigm.type](paradigm)
  val booleansInConstructor = new Booleans[CtorCtxt, paradigm.type](paradigm)

  val doublesInMethod =
    new Arithmetic[MethodBodyCtxt, Double, paradigm.type](
      paradigm,
      TypeRep.Double,
      Type.Name("Double"),
      Lit.Double(_)
    )
  val doublesInConstructor =
    new Arithmetic[CtorCtxt, Double, paradigm.type](
      paradigm,
      TypeRep.Double,
      Type.Name("Double"),
      Lit.Double(_)
    )
  val realDoublesInMethod =
    new RealArithmetic[MethodBodyCtxt, Double, paradigm.type](
      paradigm,
      TypeRep.Double,
      scala.meta.Type.Name("Double"),
      scala.meta.Lit.Double(_)
    )
  val realDoublesInConstructor =
    new RealArithmetic[MethodBodyCtxt, Double, paradigm.type](
      paradigm,
      TypeRep.Double,
      scala.meta.Type.Name("Double"),
      scala.meta.Lit.Double(_)
    )

  val intsInMethod =
    new Arithmetic[MethodBodyCtxt, Int, paradigm.type](
      paradigm,
      TypeRep.Int,
      Type.Name("Int"),
      Lit.Int(_)
    )
  val intsInConstructor =
    new Arithmetic[CtorCtxt, Int, paradigm.type](
      paradigm,
      TypeRep.Int,
      Type.Name("Int"),
      Lit.Int(_)
    )

  val stringsInMethod =
    new Strings[MethodBodyCtxt, paradigm.type](paradigm)
  val stringsInConstructor =
    new Strings[CtorCtxt, paradigm.type](paradigm)

  val equalityInMethod =
    new Equality[MethodBodyCtxt, paradigm.type](paradigm)
  val equalityInConstructor =
    new Equality[CtorCtxt, paradigm.type](paradigm)

  val listsInMethod =
    new Lists[MethodBodyCtxt, paradigm.type](paradigm)

  val listsInConstructor =
    new Lists[CtorCtxt, paradigm.type](paradigm)

  /*val treesInMethod =
    Trees[MethodBodyCtxt, paradigm.type, ObjectOriented](
      paradigm,
      paradigm.methodBodyCapabilities.canAddImportInMethodBody
    )(ooParadigm)

  val treesInConstructor =
    Trees[CtorCtxt, paradigm.type, ObjectOriented](
      paradigm,
      ooParadigm.constructorCapabilities.canAddImportInConstructor
    )(ooParadigm) */

  val assertionsInMethod = new Assertions[paradigm.type](paradigm)(functional)
}

object CodeGenerator {

  case object Enable extends Command {
    type Result = Unit
  }

  val defaultConfig: Config =
    Config(
      targetPackage = Pkg(Term.Name("ep"), List.empty),
      projectName = None
    )

  def apply(config: Config = defaultConfig): CodeGenerator =
    new CodeGenerator(config)
}
