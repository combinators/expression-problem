package example.expression.cpp

import example.expression.j.Operators
import org.combinators.templating.twirl.Java
import shared.compilation.CodeGeneratorRegistry

/**
  * Used solely to instantiate expression instances that are restricted to {Lit, BinaryExp, UnaryExp}.
  *
  * Not extensible to new data types that fall outside of the Lit/BinaryExp/UnaryExp hierarchy.
  */
trait InstanceCodeGenerators extends Operators with InstanceContext {

  /**
    * Code generator for building up the structure of the expression using classes
    *
    * new BinaryExp(new Add, new Lit(new Lit, 1), new Lit(new Lit, 2))  -->
    *   Add add = new Add(new Lit(1), new Lit(2));
    *
    *    // ( ( 5 * 7 ) + ( 18 / 9 ) )
    *
    * C++ solution requires more work:
    * double val1 = 5;
    * double val2 = 7;
    * double val3 = 18;
    * double val4 = 9;
    * Lit lit1 = Lit(&val1);
    * Lit lit2 = Lit(&val2);
    * Lit lit3 = Lit(&val3);
    * Lit lit4 = Lit(&val4);
    * Mult mult5 = Mult(&lit1, &lit2);
    * Divd divd6 = Divd(&lit3, &lit4);
    * Add add7 = Add(&mult5, &divd6);
    */
  object defaultInstance {
    val instanceGenerators: CodeGeneratorRegistry[Context] = CodeGeneratorRegistry.merge[Context](
      CodeGeneratorRegistry[Context, expression.instances.Lit] {
        case (_: CodeGeneratorRegistry[Context], lit: expression.instances.Lit) =>
          new LitContext ("lit", "Lit", lit.value)
      },

      CodeGeneratorRegistry[Context, expression.instances.BinaryExp] {
        case (registry: CodeGeneratorRegistry[Context], binary: expression.instances.BinaryExp) =>
          val Type: String = binary.self.getClass.getSimpleName
          val left: Option[Context] = registry(binary.left)
          val right: Option[Context] = registry(binary.right)
          if (left.isDefined && right.isDefined) {
            new BinaryContext(Type.toLowerCase, Type, left.get, right.get)
          } else {
            // not sure what to return
            new EmptyContext(binary.op.getClass.getSimpleName
            )
          }
      },

      CodeGeneratorRegistry[Context, expression.instances.UnaryExp] {
        case (registry: CodeGeneratorRegistry[Context], unary: expression.instances.UnaryExp) =>
          val Type: String = unary.self.getClass.getSimpleName
          val inner: Option[Context] = registry(unary.exp)
          if (inner.isDefined) {
             new UnaryContext(Type.toLowerCase, Type, inner.get)
          } else {
            new EmptyContext(unary.exp.getClass.getSimpleName)
          }
      },
    )
  }
}
