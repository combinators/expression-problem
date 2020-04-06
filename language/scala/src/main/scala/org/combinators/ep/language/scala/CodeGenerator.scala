package org.combinators.ep.language.scala

import cats.{Apply => _}
import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.generator.Command
import org.combinators.ep.language.scala.paradigm._
//import org.combinators.ep.language.scala.paradigm.ffi._

import scala.meta._

/**
  * Java-specific.
  *
  * These paradigm-specific traits are conceptually different from each other
  */
sealed class CodeGenerator(config: Config) { cc =>
  val paradigm: AnyParadigm = AnyParadigm(config)
  /*val ooParadigm: ObjectOriented[paradigm.type] = ObjectOriented(paradigm)
  val imperativeInMethod: Imperative[MethodBodyCtxt, paradigm.type] = Imperative.inMethodContext(paradigm)
  val imperativeInConstructor: Imperative[CtorCtxt, paradigm.type] = Imperative.inConstructorContext(paradigm)
  val parametricPolymorphism: ParametricPolymorphism[paradigm.type] = ParametricPolymorphism(paradigm)
  val generics: Generics.Aux[paradigm.type, ooParadigm.type, parametricPolymorphism.type] = Generics(paradigm)(ooParadigm, parametricPolymorphism)

  val booleansInMethod = new Booleans[MethodBodyCtxt, paradigm.type](paradigm)
  val booleansInConstructor = new Booleans[MethodBodyCtxt, paradigm.type](paradigm)

  val doublesInMethod =
    new Arithmetic[MethodBodyCtxt, Double, paradigm.type](
      paradigm,
      TypeRep.Double,
      Java("double").tpe(),
      new DoubleLiteralExpr(_)
    )
  val doublesInConstructor =
    new Arithmetic[CtorCtxt, Double, paradigm.type](
      paradigm,
      TypeRep.Double,
      Java("double").tpe(),
      new DoubleLiteralExpr(_)
    )

  val intsInMethod =
    new Arithmetic[MethodBodyCtxt, Int, paradigm.type](
      paradigm,
      TypeRep.Int,
      Java("int").tpe(),
      new IntegerLiteralExpr(_)
    )
  val intsInConstructor =
    new Arithmetic[CtorCtxt, Int, paradigm.type](
      paradigm,
      TypeRep.Int,
      Java("int").tpe(),
      new IntegerLiteralExpr(_)
    )

  val stringsInMethod =
    new Strings[MethodBodyCtxt, paradigm.type](
      paradigm,
      ooParadigm.methodBodyCapabilities.canGetMemberInMethod,
      paradigm.methodBodyCapabilities.canApplyInMethodBody
    )
  val stringsInConstructor =
    new Strings[CtorCtxt, paradigm.type](
      paradigm,
      ooParadigm.constructorCapabilities.canGetMemberInConstructor,
      ooParadigm.constructorCapabilities.canApplyInConstructor
    )

  val equalityInMethod =
    new Equality[MethodBodyCtxt, paradigm.type](
      paradigm,
      ooParadigm.methodBodyCapabilities.canGetMemberInMethod,
      paradigm.methodBodyCapabilities.canApplyInMethodBody
    )
  val equalityInConstructor =
    new Equality[CtorCtxt, paradigm.type](
      paradigm,
      ooParadigm.constructorCapabilities.canGetMemberInConstructor,
      ooParadigm.constructorCapabilities.canApplyInConstructor
    )

  val listsInMethod =
    Lists[MethodBodyCtxt, paradigm.type, generics.type](
      paradigm,
      ooParadigm.methodBodyCapabilities.canGetMemberInMethod,
      paradigm.methodBodyCapabilities.canApplyInMethodBody,
      parametricPolymorphism.methodBodyCapabilities.canApplyTypeInMethod,
      paradigm.methodBodyCapabilities.canAddImportInMethodBody
    )(generics)

  val listsInConstructor =
    Lists[CtorCtxt, paradigm.type, generics.type](
      paradigm,
      ooParadigm.constructorCapabilities.canGetMemberInConstructor,
      ooParadigm.constructorCapabilities.canApplyInConstructor,
      generics.constructorCapabilities.canApplyTypeInConstructor,
      ooParadigm.constructorCapabilities.canAddImportInConstructor
    )(generics)

  val treesInMethod =
    Trees[MethodBodyCtxt, paradigm.type, ObjectOriented](
      paradigm,
      paradigm.methodBodyCapabilities.canAddImportInMethodBody
    )(ooParadigm)

  val treesInConstructor =
    Trees[CtorCtxt, paradigm.type, ObjectOriented](
      paradigm,
      ooParadigm.constructorCapabilities.canAddImportInConstructor
    )(ooParadigm)

  val assertionsInMethod = new Assertions[paradigm.type](paradigm)(ooParadigm) */
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
