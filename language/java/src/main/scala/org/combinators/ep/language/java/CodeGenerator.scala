package org.combinators.ep.language.java     /*DI:LD:AI*/

import cats.{Apply => _}
import com.github.javaparser.ast.PackageDeclaration
import com.github.javaparser.ast.`type`.PrimitiveType
import com.github.javaparser.ast.expr.{DoubleLiteralExpr, IntegerLiteralExpr, TypeExpr}
import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.generator.Command
import org.combinators.ep.language.java.paradigm._
import org.combinators.ep.language.java.paradigm.ffi._


/**
 * Java-specific.
 *
 * These paradigm-specific traits are conceptually different from each other
 */
sealed class CodeGenerator(config: Config) { cc =>
  val paradigm: AnyParadigm = AnyParadigm(config)
  val ooParadigm: ObjectOriented[paradigm.type] = ObjectOriented(paradigm)
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
      PrimitiveType.doubleType(),
      new DoubleLiteralExpr(_)
    )
  val doublesInConstructor =
    new Arithmetic[CtorCtxt, Double, paradigm.type](
      paradigm,
      TypeRep.Double,
      PrimitiveType.doubleType(),
      new DoubleLiteralExpr(_)
    )
  val realDoublesInMethod =
    new RealArithmetic[MethodBodyCtxt, Double, paradigm.type](
      paradigm,
      TypeRep.Double,
      PrimitiveType.doubleType(),
      new DoubleLiteralExpr(_)
    )
  val realDoublesInConstructor =
    new RealArithmetic[MethodBodyCtxt, Double, paradigm.type](
      paradigm,
      TypeRep.Double,
      PrimitiveType.doubleType(),
      new DoubleLiteralExpr(_)
    )

  val intsInMethod =
    new Arithmetic[MethodBodyCtxt, Int, paradigm.type](
      paradigm,
      TypeRep.Int,
      PrimitiveType.intType(),
      x => new IntegerLiteralExpr(String.valueOf(x))
    )
  val intsInConstructor =
    new Arithmetic[CtorCtxt, Int, paradigm.type](
      paradigm,
      TypeRep.Int,
      PrimitiveType.intType(),
      x => new IntegerLiteralExpr(String.valueOf(x))
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

  val consoleInMethod =
    new Console[MethodBodyCtxt, paradigm.type](
      paradigm, stringsInMethod
    )

  val consoleInConstructor =
    new Console[CtorCtxt, paradigm.type](
      paradigm, stringsInConstructor
    )
  
  val arraysInMethod =
    new Arrays[MethodBodyCtxt, paradigm.type](
      paradigm
    )

  val arraysInConstructor =
    new Arrays[CtorCtxt, paradigm.type](
      paradigm
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

  val assertionsInMethod = new Assertions[paradigm.type](paradigm)(ooParadigm)
  val exceptionsInMethod = new Exceptions[paradigm.type](paradigm)
}

object CodeGenerator {

  case object Enable extends Command {
    type Result = Unit
  }

  val defaultConfig: Config =
    Config(
      targetPackage = new PackageDeclaration(ObjectOriented.fromComponents("ep")),
      projectName = None,
      boxLevel = FullyBoxed
    )

  def apply(config: Config = defaultConfig): CodeGenerator =
    new CodeGenerator(config)
}
