package org.combinators.ep.language.java     /*DI:LD:AI*/

import cats.{Apply => _}
import com.github.javaparser.ast.PackageDeclaration
import com.github.javaparser.ast.`type`.PrimitiveType
import com.github.javaparser.ast.expr.{DoubleLiteralExpr, IntegerLiteralExpr}
import org.combinators.cogen.TypeRep
import org.combinators.cogen.Command
import org.combinators.ep.language.java.paradigm._
import org.combinators.ep.language.java.paradigm.ffi._

/**
 * Java-specific.
 *
 * These paradigm-specific traits are conceptually different from each other
 */
class CodeGenerator(config: Config) { cc =>
  val paradigm: AnyParadigm = AnyParadigm(config)
  val ooParadigm: ObjectOriented[paradigm.type] = ObjectOriented(paradigm)
  val imperativeInMethod: Imperative[MethodBodyCtxt, paradigm.type] = Imperative.inMethodContext(paradigm)
  val imperativeInConstructor: Imperative[CtorCtxt, paradigm.type] = Imperative.inConstructorContext(paradigm)
  val parametricPolymorphism: ParametricPolymorphism[paradigm.type] = ParametricPolymorphism(paradigm)
  val generics: Generics.Aux[paradigm.type, ooParadigm.type, parametricPolymorphism.type] = Generics(paradigm)(ooParadigm, parametricPolymorphism)

  val booleansInMethod = new Booleans[MethodBodyCtxt, Boolean, paradigm.type](paradigm)
  val booleansInConstructor = new Booleans[MethodBodyCtxt, Boolean, paradigm.type](paradigm)

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
    new Strings[MethodBodyCtxt, String, paradigm.type](
      paradigm,
      ooParadigm.methodBodyCapabilities.canGetMemberInMethod,
      paradigm.methodBodyCapabilities.canApplyInMethodBody
    )
  val stringsInConstructor =
    new Strings[CtorCtxt, String, paradigm.type](
      paradigm,
      ooParadigm.constructorCapabilities.canGetMemberInConstructor,
      ooParadigm.constructorCapabilities.canApplyInConstructor
    )

  val equalityInMethod =
    new Equality[MethodBodyCtxt, Unit, paradigm.type](   // Heineman: Not sure if Unit is correct
      paradigm,
      ooParadigm.methodBodyCapabilities.canGetMemberInMethod,
      paradigm.methodBodyCapabilities.canApplyInMethodBody
    )
  val equalityInConstructor =
    new Equality[CtorCtxt, Unit, paradigm.type]( // Heineman: Not sure if Unit is correct
      paradigm,
      ooParadigm.constructorCapabilities.canGetMemberInConstructor,
      ooParadigm.constructorCapabilities.canApplyInConstructor
    )

  val consoleInMethod =
    new Console[MethodBodyCtxt, String, paradigm.type](
      paradigm, stringsInMethod
    )

  val consoleInConstructor =
    new Console[CtorCtxt, String, paradigm.type](
      paradigm, stringsInConstructor
    )
  
  val arraysInMethod =
    new Arrays[MethodBodyCtxt, Array[Any], paradigm.type](
      paradigm
    )

  val arraysInConstructor =
    new Arrays[CtorCtxt, Array[Any], paradigm.type](
      paradigm
    )
    

  val listsInMethod =
    Lists[MethodBodyCtxt, Any, paradigm.type, Generics](
      paradigm,
      parametricPolymorphism.methodBodyCapabilities.canApplyTypeInMethod,
      paradigm.methodBodyCapabilities.canAddImportInMethodBody
    )(generics)

  val listsInConstructor =
    Lists[CtorCtxt, Any, paradigm.type, Generics](
      paradigm,
      generics.constructorCapabilities.canApplyTypeInConstructor,
      ooParadigm.constructorCapabilities.canAddImportInConstructor
    )(generics)

  val mapsInMethod =
    Maps[MethodBodyCtxt, Map[?,?], paradigm.type, Generics](
      paradigm,
      paradigm.methodBodyCapabilities.canAddImportInMethodBody,
      parametricPolymorphism.methodBodyCapabilities.canApplyTypeInMethod,
      paradigm.methodBodyCapabilities.canGetFreshNameInMethodBody,
      imperativeInMethod.imperativeCapabilities.canDeclareVariable,
      imperativeInMethod.imperativeCapabilities.canLiftExpression,
      paradigm.methodBodyCapabilities.canAddBlockDefinitionsInMethodBody
    )(generics)

  val mapsInConstructor =
    Maps[CtorCtxt, Map[?,?], paradigm.type, Generics](
      paradigm,
      ooParadigm.constructorCapabilities.canAddImportInConstructor,
      generics.constructorCapabilities.canApplyTypeInConstructor,
      ooParadigm.constructorCapabilities.canGetFreshNameInConstructor,
      imperativeInConstructor.imperativeCapabilities.canDeclareVariable,
      imperativeInConstructor.imperativeCapabilities.canLiftExpression,
      ooParadigm.constructorCapabilities.canAddBlockDefinitionsInConstructor
    )(generics)

  val assertionsInMethod = new Assertions[String, paradigm.type](paradigm)(ooParadigm)
  val exceptionsInMethod = new Exceptions[Unit, paradigm.type](paradigm)
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
