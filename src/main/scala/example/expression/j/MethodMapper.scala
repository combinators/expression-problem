package example.expression.j

import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.stmt.Statement
import org.combinators.templating.twirl.Java
import expression.types.{FrameworkTypes, GenericType, TypeInformation, Types}
import expression.{Exp, Operation}
import org.combinators.cls.types.{Constructor, Type}

import org.combinators.cls.types._
import org.combinators.cls.types.syntax._

/**
  * These codify the semantic types used by the Expression problem.
  *
  * For any of these that are ever going to be translated directly into Java Type Names, you must
  * make them Constructor.
  */
trait MethodMapper extends Operators {

  /** Implementations for an operation. Map(op, Map(exp,MethodDecls)). */
  var implementations:Map[Class[_ <: Operation],Map[Class[_ <: Exp],MethodDeclaration]] = Map()

  /**
    * Return desired map of expressions by operation.
    *
    * @param op    Operation under consideration
    */
  def getImplementation(op:Operation):Map[Class[_ <: Exp],MethodDeclaration] = implementations(op.getClass)

  /**
    * Given an operation and a map of (Exp x String) call addImpl on each.
    *
    * Placed here for easy global access.
    *
    * @param op
    * @param map
    */
  def registerImpl (op:Operation, map:Map[Exp,String]): Unit = {
    map.keys.foreach {
      key =>
        addImpl(op, key, Java(map(key)).statements())
    }
  }


  /**
    * For the given operation, add the sequence of statements to implement for given expression subtype.
    * This dynamically maintains a map which can be inspected for the code synthesis.
    *
    * @param op      Operation under consideration
    * @param exp     Expression context
    * @param stmts   Sequence of statements that represents implementation of operation in given context.
    */
  def addImpl(op:Operation, exp:Exp, stmts:Seq[Statement]): Unit = {
    val name = exp.getClass.getSimpleName

    var map:Map[Class[_ <: Exp],MethodDeclaration] = if (implementations.contains(op.getClass)) {
      implementations(op.getClass) - exp.getClass
    } else {
      Map()
    }

    val tpe:String = Type_toString(op.`type`)
    map += (exp.getClass -> Java(
      s"""
         |public $tpe visit($name e) {
         |   ${stmts.mkString("\n")}
         |}
        """.stripMargin).methodDeclarations().head)

    implementations -= op.getClass
    implementations += (op.getClass -> map)
  }

//  /** Convert a type into its Java String equivalent. */
//  def Type_toString (ty:TypeInformation): String =
//    ty match {
//      case Types.Exp=> "Exp"           // base class of everything
//
//      case Types.Void => "void"
//      case Types.Double => "Double"      // allow boxing/unboxing for generics
//      case Types.Int => "Integer"      // allow boxing/unboxing for generics
//      case Types.String => "String"
//      case g:GenericType => Type_toString(g.base) + "<" + Type_toString(g.generic) + ">"
//      case FrameworkTypes.List => "java.util.List"
//      case _ => "None"
//    }

  /**
    * Used for code synthesis in representation.
    */
  object representation {
    def apply(part: Type): Constructor = 'Representation (part)

    val expr:Type = 'ExprGenerated    // once expressions are generated, this is the semantic type

    val eval:Type = 'EvalGen          // for Expressions
    val prettyp:Type = 'PrettyPGen    // generate statements within the excecute method of moves
  }

}
