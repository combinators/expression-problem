package org.combinators.dp.enhanced

import org.combinators.models.{BooleanType, CharType, EnhancedModel, IntegerType, StringType}
import org.combinators.cogen.TypeRep
import org.combinators.cogen.Command.Generator
import org.combinators.cogen.NameProvider
import org.combinators.cogen.paradigm.{AnyParadigm, ObjectOriented}
import org.combinators.cogen.paradigm.control.Imperative
import org.combinators.cogen.paradigm.ffi.{Arithmetic, Arrays, Assertions, Booleans, Equality, RealArithmetic, Strings}

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

  def constructedArrayType(level:Int, baseType:TypeRep): TypeRep = {
    level match {
      case 0 =>  baseType
      case value: Int =>
        val inner:TypeRep = constructedArrayType(value-1, baseType)
        TypeRep.Array(inner)

    }
  }

  def return_type_based_on_model(model:EnhancedModel) : Generator[MethodBodyContext, Type] = {
    import paradigm.methodBodyCapabilities._

    model.subproblemType match {
      case _:IntegerType => for {
        intType <- toTargetLanguageType(TypeRep.Int)
      } yield intType

      case _:CharType => for {
        charType <- toTargetLanguageType(TypeRep.Char)
      } yield charType

      case _:BooleanType => for {
        boolType <- toTargetLanguageType(TypeRep.Boolean)
      } yield boolType

      case _:StringType => for {
        strType <- toTargetLanguageType(TypeRep.String)
      } yield strType

      case _ => ???
    }
  }

//  def helper_method_type(model:EnhancedModel) : Generator[MethodBodyContext, Type] = {
//    import paradigm.methodBodyCapabilities._
//
//    model.subproblemType match {
//      case _:IntegerType => for {
//        intType <- toTargetLanguageType(TypeRep.Int)
//      } yield intType
//
//      case _:CharType => for {
//        charType <- toTargetLanguageType(TypeRep.Char)
//      } yield charType
//
//      case _:BooleanType => for {
//        boolType <- toTargetLanguageType(TypeRep.Boolean)
//      } yield boolType
//
//      case _:StringType => for {
//        strType <- toTargetLanguageType(TypeRep.String)
//      } yield strType
//
//      case _ => ???
//    }
//  }

//  def helper_method_type_in_class(model:EnhancedModel) : TypeRep = {
//
//    model.subproblemType match {
//      case _:IntegerType => TypeRep.Int
//      case _:CharType => TypeRep.Char
//      case _:BooleanType => TypeRep.Boolean
//      case _:StringType => TypeRep.String
//
//      case _ => ???
//    }
//  }

  def helper_default(model:EnhancedModel) : Generator[MethodBodyContext, Expression ] = {

        model.subproblemType match {
          case _: IntegerType => for {
            zero <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 0)
          } yield zero

          case _: BooleanType => for {
            falseValue <- paradigm.methodBodyCapabilities.reify(TypeRep.Boolean, false)
          } yield falseValue

          case _ => ???
        }
  }

  def helper_problemType(model:EnhancedModel) : TypeRep = {
    import ooParadigm.constructorCapabilities._

    model.subproblemType match {
      case _:IntegerType => TypeRep.Int
      case _:CharType => TypeRep.Char
      case _:BooleanType => TypeRep.Boolean
      case _:StringType => TypeRep.String

      case _ => ???
    }
  }
}
