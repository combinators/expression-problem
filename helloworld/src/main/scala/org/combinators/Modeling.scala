package org.combinators

import com.github.javaparser.ast.expr.DoubleLiteralExpr
import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.NameProvider
import org.combinators.ep.generator.paradigm.{AnyParadigm, ObjectOriented}
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Assertions, Booleans, Equality, RealArithmetic}

// Expression Tree
sealed abstract class Condition(val label:String)

final case class True() extends Condition(label = "true")
final case class False() extends Condition(label = "false")

final case class LT(left:Formula, right:Formula) extends Condition(label = "lt")
final case class LE(left:Formula, right:Formula) extends Condition(label = "le")

final case class Not(inner:Condition) extends Condition(label = "not")
final case class And(left:Condition, right:Condition) extends Condition(label = "and")
final case class Or(left:Condition, right:Condition) extends Condition(label = "or")

// Operations
abstract class Formula(val label:String)
case class ConstantFormula(value:Double, actualType:TypeRep) extends Formula(label="const")
case class VariableFormula(name:String, actualType:TypeRep) extends Formula(label="var")
final case class Pi() extends Formula(label = "pi")
final case class Euler() extends Formula(label = "euler")

case class DoubleToInt(inner:Formula) extends Formula("cast")

case class Sqrt(inner:Formula) extends Formula(label = "sqrt")
case class Sin(inner:Formula) extends Formula(label = "sin")
case class Cos(inner:Formula) extends Formula(label = "cos")
case class Floor(inner:Formula) extends Formula(label = "floor")
case class Abs(inner:Formula) extends Formula(label = "abs")

case class Add(left:Formula, right:Formula) extends Formula("add")
case class Sub(left:Formula, right:Formula) extends Formula("sub")
case class Mult(left:Formula, right:Formula) extends Formula("mult")
case class Div(left:Formula, right:Formula) extends Formula("div")
case class Mod(leftt:Formula, right:Formula) extends Formula("mod")
case class Pow(left:Formula, right:Formula) extends Formula("pow")
case class Log(left:Formula, right:Formula) extends Formula("log")

case class UnaryFunctionCall(name:String, inner:Formula) extends Formula(label="function")
case class BinaryFunctionCall(name:String, left:Formula, right:Formula) extends Formula(label="function")

class Structure(val conditions:Seq[(Condition, Formula)], val default:Formula) {

}

trait Expansion {
  val paradigm: AnyParadigm
  val names: NameProvider[paradigm.syntax.Name]

  val ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Int]
  val ffiRealArithmetic : RealArithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double]
  val ffiBooleans : Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val ffiAssertions : Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val ffiEquality : Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]

  // must be provided
  def find_method_recursive(name:paradigm.syntax.Name) : Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression]
  def cast_double_to_int(inner:paradigm.syntax.Expression) :  Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression]


  def expand(f:Formula): Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression] = {
    import paradigm.methodBodyCapabilities._

    f match {

      case DoubleToInt(inner) =>
        for {
          in <- expand(inner)
          res <- cast_double_to_int(in)
        } yield res

      case ConstantFormula(value, TypeRep.Int) =>
        for {
          value <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, value.intValue())
        } yield value

      case ConstantFormula(value, TypeRep.Double) =>
        for {
          value <- paradigm.methodBodyCapabilities.reify(TypeRep.Double, value)
        } yield value

        // this works for Java. NOT for Scala
      case VariableFormula(name, typeRep) =>
        for {
          args <- getArguments()

          specific = args.filter(triple => triple._1.toString.equals(name)).head
        } yield specific._3

      case Pi() =>
        for {
          value <- paradigm.methodBodyCapabilities.reify(TypeRep.Double, math.Pi)
        } yield value

      case Euler() =>
        for {
          value <- paradigm.methodBodyCapabilities.reify(TypeRep.Double, math.E)
        } yield value

      case Sqrt(inner) =>
        for {
          in <- expand(inner)
          value <- ffiRealArithmetic.realArithmeticCapabilities.sqrt(in)
        } yield value

      case Sin(inner) =>
        for {
          in <- expand(inner)
          value <- ffiRealArithmetic.realArithmeticCapabilities.sin(in)
        } yield value

      case Cos(inner) =>
        for {
          in <- expand(inner)
          value <- ffiRealArithmetic.realArithmeticCapabilities.cos(in)
        } yield value

      case Floor(inner) =>
        for {
          in <- expand(inner)
          value <- ffiRealArithmetic.realArithmeticCapabilities.floor(in)
        } yield value

      case Abs(inner) =>
        for {
          in <- expand(inner)
          value <- ffiRealArithmetic.realArithmeticCapabilities.abs(in)
        } yield value

      case Add(left, right) =>
        for {
          leftF <- expand(left)
          rightF <- expand(right)
          value <- ffiArithmetic.arithmeticCapabilities.add(leftF, rightF)
        } yield value

      case Sub(left, right) =>
        for {
          leftF <- expand(left)
          rightF <- expand(right)
          value <- ffiArithmetic.arithmeticCapabilities.sub(leftF, rightF)
        } yield value

      case Mult(left, right) =>
        for {
          leftF <- expand(left)
          rightF <- expand(right)
          value <- ffiArithmetic.arithmeticCapabilities.mult(leftF, rightF)
        } yield value

      case Div(left, right) =>
        for {
          leftF <- expand(left)
          rightF <- expand(right)
          value <- ffiArithmetic.arithmeticCapabilities.div(leftF, rightF)
        } yield value

      case Mod(left, right) =>
        for {
          leftF <- expand(left)
          rightF <- expand(right)
          value <- ffiArithmetic.arithmeticCapabilities.mod(leftF, rightF)
        } yield value

      case Pow(left, right) =>
        for {
          leftF <- expand(left)
          rightF <- expand(right)
          value <- ffiRealArithmetic.realArithmeticCapabilities.pow(leftF, rightF)
        } yield value

      case Log(left, right) =>
        for {
          leftF <- expand(left)
          rightF <- expand(right)
          value <- ffiRealArithmetic.realArithmeticCapabilities.log(leftF, rightF)
        } yield value

      case UnaryFunctionCall(name, inner) =>
        for {
          func <- find_method_recursive(names.mangle(name))
          in <- expand(inner)
          fres <- apply(func, Seq(in))
        } yield fres

      case BinaryFunctionCall(name, left, right) =>
        for {
          func <- find_method_recursive(names.mangle(name))
          leftF <- expand(left)
          rightF <- expand(right)
          fres <- apply(func, Seq(leftF, rightF))
        } yield fres
    }
  }

  def expand(cond:Condition): Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression] = {
    import paradigm.projectCapabilities._

    cond match {
           case True() =>
        for {
          res <- ffiBooleans.booleanCapabilities.trueExp
        } yield res

      case False() =>
        for {
          res <- ffiBooleans.booleanCapabilities.falseExp
        } yield res

      case Not(inner) =>
        for {
          in <- expand(inner)
          res <- ffiBooleans.booleanCapabilities.not(in)
        } yield res

      case And(left, right) =>
        for {
          leftExp <- expand(left)
          rightExp <- expand(right)
          res <- ffiBooleans.booleanCapabilities.and(Seq(leftExp, rightExp))
        } yield res

      case Or(left, right) =>
        for {
          leftExp <- expand(left)
          rightExp <- expand(right)
          res <- ffiBooleans.booleanCapabilities.or(Seq(leftExp, rightExp))
        } yield res

      case LT(left, right) =>
        for {
          leftExp <- expand(left)
          rightExp <- expand(right)
          res <- ffiArithmetic.arithmeticCapabilities.lt(leftExp, rightExp)
        } yield res

      case LE(left, right) =>
        for {
          leftExp <- expand(left)
          rightExp <- expand(right)
          res <- ffiArithmetic.arithmeticCapabilities.le(leftExp, rightExp)
        } yield res

      case _ => ???
    }
  }


}