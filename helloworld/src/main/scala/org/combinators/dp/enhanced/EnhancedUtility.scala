package org.combinators.dp.enhanced

import org.combinators.dp.GenerationOption
import org.combinators.model.{BooleanType, EnhancedModel, IntegerType, StringType}
import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{FileWithPath, NameProvider}
import org.combinators.ep.generator.paradigm.{AnyParadigm, ObjectOriented}
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Arrays, Assertions, Booleans, Equality, RealArithmetic, Strings}

trait EnhancedUtility {
  val paradigm: AnyParadigm
  val names: NameProvider[paradigm.syntax.Name]
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val impParadigm: Imperative.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val arithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double]
  val realArithmetic: RealArithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double]
  val array: Arrays.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val eqls: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val asserts: Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val strings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val booleans: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type]

  import paradigm._
  import syntax._
  import ooParadigm._

  def return_type_based_on_model(model:EnhancedModel) : Generator[MethodBodyContext, Type] = {
    import paradigm.methodBodyCapabilities._

    model.subproblemType match {
      case _:IntegerType => for {
        intType <- toTargetLanguageType(TypeRep.Int)
      } yield intType

      case _:BooleanType => for {
        boolType <- toTargetLanguageType(TypeRep.Boolean)
      } yield boolType

      case _:StringType => for {
        strType <- toTargetLanguageType(TypeRep.String)
      } yield strType

      case _ => ???
    }
  }

  def helper_method_type(model:EnhancedModel) : Generator[MethodBodyContext, Type] = {
    import paradigm.methodBodyCapabilities._

    model.subproblemType match {
      case _:IntegerType => for {
        intType <- toTargetLanguageType(TypeRep.Int)
      } yield intType

      case _:BooleanType => for {
        boolType <- toTargetLanguageType(TypeRep.Boolean)
      } yield boolType

      case _:StringType => for {
        strType <- toTargetLanguageType(TypeRep.String)
      } yield strType

      case _ => ???
    }
  }

  def helper_method_type_in_class(model:EnhancedModel) : Generator[ClassContext, Type] = {
    import ooParadigm.classCapabilities._

    model.subproblemType match {
      case _:IntegerType => for {
        intType <- toTargetLanguageType(TypeRep.Int)
      } yield intType

      case _:BooleanType => for {
        boolType <- toTargetLanguageType(TypeRep.Boolean)
      } yield boolType

      case _:StringType => for {
        strType <- toTargetLanguageType(TypeRep.String)
      } yield strType

      case _ => ???
    }
  }

  def helper_method_type_in_constructor(model:EnhancedModel) : Generator[ConstructorContext, Type] = {
    import ooParadigm.constructorCapabilities._

    model.subproblemType match {
      case _:IntegerType => for {
        intType <- toTargetLanguageType(TypeRep.Int)
      } yield intType

      case _:BooleanType => for {
        boolType <- toTargetLanguageType(TypeRep.Boolean)
      } yield boolType

      case _:StringType => for {
        strType <- toTargetLanguageType(TypeRep.String)
      } yield strType

      case _ => ???
    }
  }
}