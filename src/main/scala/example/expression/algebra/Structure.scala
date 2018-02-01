package example.expression.algebra

import com.github.javaparser.ast.CompilationUnit
import example.expression.j.MethodMapper
import example.expression.{Base, ExpressionDomain}
import expression._
import expression.data.{Add, Eval, Lit}
import expression.extensions._
import expression.types.Types
import org.combinators.cls.interpreter.{ReflectedRepository, combinator}
import org.combinators.cls.types.Type
import org.combinators.cls.types.syntax._
import org.combinators.templating.twirl.Java

import scala.collection.JavaConverters._

/** Use Modularity2016 Java solution. Built from same domain model. */
trait Structure extends Base with SemanticTypes with MethodMapper {

  /** Add dynamic combinators as needed. */
  override def init[G <: ExpressionDomain](gamma: ReflectedRepository[G], model: DomainModel): ReflectedRepository[G] = {
    var updated = super.init(gamma, model)

    updated
  }


    // implementations of operations: have to be defined. Note that these "raw Scala methods" could be replaced with tabular tool
    //
    //
    //  Eval     x  Lit, Neg, Add, Sub  ...  Mult Divide ...
    //  Print    |  pl,  pn,  pa,  ps
    //  Collect  |  cl,  cn,  ca,  cs   ...
    //  Simplify |  sl,  sn,  sa,  ss   ...


    def registerImpl (op:Operation, map:Map[Exp,String]): Unit = {
      map.keys.foreach {
        key =>
          addImpl(op, key, Java(map(key)).statements())
      }
    }

    registerImpl(new Eval, Map(

      new Lit -> "return e.getValue();",
      new Add -> "return e.getLeft().accept(this) + e.getRight().accept(this);",
      new Sub -> "return e.getLeft().accept(this) - e.getRight().accept(this);",
      new Mult -> "return e.getLeft().accept(this) * e.getRight().accept(this);",
      new Divd -> "return e.getLeft().accept(this) / e.getRight().accept(this);",
      new Neg -> "return -e.getExp().accept(this);"
    ))

  registerImpl(new PrettyP, Map(
    new Lit -> """return "" + e.getValue();""",
    new Add -> """return "(" + e.getLeft().accept(this) + "+" + e.getRight().accept(this) + ")";""",
    new Sub -> """return "(" + e.getLeft().accept(this) + "-" + e.getRight().accept(this) + ")";""",
    new Mult -> """return "(" + e.getLeft().accept(this) + "*" + e.getRight().accept(this) + ")";""",
    new Divd -> """return "(" + e.getLeft().accept(this) + "/" + e.getRight().accept(this) + ")";""",
    new Neg -> """return "-" + e.getExp().accept(this);"""
  ))




  // sample Driver
  @combinator object Driver {
    def apply:CompilationUnit = Java(s"""
         |package ep;
         |
         |public class Driver {
         |  public static void main(String[] args) {
         |    System.out.println("======Add======");
         |    Add add = new AddFinal(new LitFinal(7), new LitFinal(4));
         |    System.out.println(add.eval());
         |    System.out.println("======Sub======");
         |    Sub sub = new SubFinal(new LitFinal(7), new LitFinal(4));
         |    System.out.println(sub.eval());
         |    System.out.println("======Print======");
         |
         |    /* the line below causes compile-time error, if now commented out. */
         |    //AddPrettyPFinal exp = new AddPrettyPFinal(new LitFinal(7)), new LitFinal(4));
         |    AddPrettyPFinal prt = new AddPrettyPFinal(new LitPrettyPFinal(7), new LitPrettyPFinal(4));
         |    System.out.println(prt.print() + " = " + prt.eval());
         |    System.out.println("======CollectLiterals======");
         |    AddCollectFinal addc = new AddCollectFinal(new LitCollectFinal(3), new LitCollectFinal(4));
         |    System.out.println(addc.collectList().toString());
         |    System.out.println("======Composition: Independent Extensibility======");
         |    AddPrettyPCollectFinal addpc = new AddPrettyPCollectFinal(new LitPrettyPCollectFinal(3), new LitPrettyPCollectFinal(4));
         |    System.out.println(addpc.print() + " = " + addpc.eval() + " Literals: " + addpc.collectList().toString());
         |  }
         |}""".stripMargin).compilationUnit()

    val semanticType:Type = driver
  }

}
