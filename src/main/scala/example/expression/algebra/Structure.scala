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

    // for now, this still uses the visitor structure that is shared. Must fix eventually

//    def registerImpl (op:Operation, map:Map[Exp,String]): Unit = {
//      map.keys.foreach {
//        key =>
//          addImpl(op, key, Java(map(key)).statements())
//      }
//    }

  registry.registerImpl(new Eval, Map(

      new Lit -> "return value;",
      new Add -> "return left.eval() + right.eval();",
      new Sub -> "return left.eval() - right.eval();",
      new Mult -> "return left.eval() * right.eval();",
      new Divd -> "return left.eval() / right.eval();",
      new Neg -> "return -value; /*HACK */"
    ))

  registry.registerImpl(new PrettyP, Map(
    new Lit -> """return "" + value;""",
    new Add -> """return "(" + left.prettyp() + "+" + right.prettyp()+ ")";""",
    new Sub -> """return "(" + left.prettyp() + "-" + right.prettyp() + ")";""",
    new Mult -> """return left.prettyp() + " * " + right.prettyp();""",
    new Divd -> """return "(" + left.prettyp() + "/" + right.prettyp() + ")";""",
    new Neg -> """return "-" + value.prettyp(); /**HACK**/"""
  ))




  // sample Driver
  @combinator object Driver {
    def apply:CompilationUnit = Java(s"""
         |package algebra;
         |
         |public class Driver {
         |  public static void main(String[] args) {
         |
         |        // need some structure to represent 3 + 5
         |        ExpAlg addAlgebra = new EvalExpAlg();
         |        Eval lit3 = (Eval) addAlgebra.lit(3);
         |        Eval lit5 = (Eval) addAlgebra.lit(5);
         |
         |        System.out.println ("Lit3 = " + lit3.eval());
         |        Eval add = (Eval) addAlgebra.add(lit3, lit5);
         |        System.out.println ("Add = " + add.eval());
         |
         |        PrettyPExpAlg printAlgebra = new PrettyPExpAlg();
         |        PrettyP plit3 = (PrettyP) printAlgebra.lit(3);
         |        PrettyP plit5 = (PrettyP) printAlgebra.lit(5);
         |        PrettyP padd = (PrettyP) printAlgebra.add(plit3, plit5);
         |
         |        System.out.println ("Padd = " + padd.prettyp());
         |
         |        EvalSubExpAlg subAlgebra = new EvalSubExpAlg();
         |        //      Eval slit3 = (Eval) subAlgebra.lit(3);
         |        //      Eval slit5 = (Eval) subAlgebra.lit(5);
         |
         |        Eval sub = (Eval) subAlgebra.sub(lit3, lit5);
         |        System.out.println ("Sub = " + sub.eval());
         |
         |        PrettyPMultExpAlg multAlgebra = new PrettyPMultExpAlg();
         |        PrettyP mlit6 = (PrettyP) multAlgebra.lit(6);
         |        PrettyP mlit7 = (PrettyP) multAlgebra.lit(7);
         |        PrettyP madd = (PrettyP) multAlgebra.add(mlit6, mlit7);
         |
         |        /** reuse older concepts. **/
         |        PrettyP finalOne = (PrettyP) multAlgebra.mult(madd, padd);
         |        System.out.println ("Final = " + finalOne.prettyp());
         |
         |  }
         |}""".stripMargin).compilationUnit()

    val semanticType:Type = driver
  }

}
