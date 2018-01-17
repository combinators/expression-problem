package example.expression.cpp

import expression.types.{FrameworkTypes, GenericType, TypeInformation, Types}
import expression.{Exp, Operation}
import org.combinators.cls.types._
import org.combinators.cls.types.syntax._
import example.expression.covariant.SemanticTypes

/**
  * These codify the semantic types used by the Expression problem.
  *
  * For any of these that are ever going to be translated directly into Java Type Names, you must
  * make them Constructor.
  */
trait CPPSemanticTypes extends SemanticTypes {

  /** Implementations for an operation. Map(op, Map(exp,String)). */
  var implementations:Map[Class[_ <: Operation],Map[Class[_ <: Exp],String]] = Map()

  /**
    * Return desired map of expressions by operation.
    *
    * @param op    Operation under consideration
    */
  def getImplementation(op:Operation):Map[Class[_ <: Exp],String] = implementations(op.getClass)

  /**
    * For the given operation, add the sequence of statements to implement for given expression subtype.
    * This dynamically maintains a map which can be inspected for the code synthesis.
    *
    * @param op     Operation under consideration
    * @param exp    Expression context
    * @param stmts  Statements that provides the desired implementation in context
    */
  def addImpl(op:Operation, exp:Exp, stmts:String): Unit = {
    val name = exp.getClass.getSimpleName

    var map:Map[Class[_ <: Exp],String] = if (implementations.contains(op.getClass)) {
      implementations(op.getClass) - exp.getClass
    } else {
      Map()
    }

    map += (exp.getClass ->
      s"""
         |void Visit${exp.getClass.getSimpleName}(const $name* e) {
         |   $stmts
         |}
        """.stripMargin)

    implementations -= op.getClass
    implementations += (op.getClass -> map)
  }

  /** Convert a type into its Java String equivalent. */
  def Type_toString (ty:TypeInformation): String =
    ty match {
      case Types.Exp=> "Exp*"           // base class of everything (turn into pointer)

      case Types.Void => "void"
      case Types.Int => "int"      // allow boxing/unboxing for generics
      case Types.String => "std::string"
      case g:GenericType => Type_toString(g.base) + "<" + Type_toString(g.generic) + ">"
      case FrameworkTypes.List => "std::vector"
      case _ => "None"
    }

  object module {
    def apply (part:Type) : Constructor = 'Module(part)

    // defines Exp and ExpVisitor classes.
    val base: Type = 'Base
  }

}
