package example.expression.algebra

import example.expression.j.MethodMapper
import expression.{Exp, Operation}
import org.combinators.cls.types.syntax._
import org.combinators.cls.types.{Constructor, Type}


/**
  * These codify the semantic types used by the Expression problem as necessary
  * for the Algebra solution
  *
  * Extensibility for the Masses.
  * Practical Extensibility with Object Algebras. Bruno C. d. S. Oliveira and William R. Cook
  *
  * For any of these that are ever going to be translated directly into Java Type Names, you must
  * make them Constructor.
  */
trait SemanticTypes extends MethodMapper {

  val driver:Type = 'Driver

  // meta-concerns. When you have completed the definition of a constructor
  object generated {
    def apply (uniq:Type) : Constructor = 'Generated(uniq)

    val complete: Type = 'Complete
    val initialized: Type = 'Initialized
  }
//
//  object alg {
//    def apply (phase:Type) : Constructor = 'Alg(phase)
//
//    val base:Type = 'Base
//  }

  object exp {
    def apply (phase:Type, exp:Exp) : Constructor =  'Exp(phase, Constructor(exp.getClass.getSimpleName))
    val base:Type = 'Base           // initial class
  }

  object evolved_exp {
    def apply (phase:Type, exp:Exp, parent:String) : Constructor =
      'EvolvedExp(phase, Constructor(exp.getClass.getSimpleName), Constructor(parent))
    val base:Type = 'Base           // initial class
  }


  /**
    * Each operation has its own interface, assuming operation names are valid Java SimpleNames.
    */
  object ops {
    def apply (phase:Type, op:Operation) : Constructor = 'Ops(phase, Constructor(op.getClass.getSimpleName))

    val algebra:Type = 'Alg         // i.e., EvalExpAlg
    val base:Type = 'Base           // initial interface Eval
  }

  object evolved_ops {
    def apply (phase:Type, op:Operation, exp:Exp, parent:String) : Constructor =
      'EvolvedOps(phase, Constructor(op.getClass.getSimpleName), Constructor(exp.getClass.getSimpleName), Constructor(parent))

    val base:Type = 'Base           // initial interface Eval
  }

  object evolved2_ops {
    def apply (phase:Type, op:Operation, parent:String) : Constructor =
      'Evolved2Ops(phase, Constructor(op.getClass.getSimpleName), Constructor(parent))
    val base:Type = 'Base           // initial interface Eval
  }

  /**
    * Types appear here
    */
  object data {
    def apply (uniq:String) : Constructor = 'Data(Constructor(uniq))
  }

  object ep {
    def apply (phase:Type, exp:Exp) : Constructor = 'Ep(phase, Constructor(exp.getClass.getSimpleName))
    def apply (phase:Type, op:Operation) : Constructor = 'Op(phase, Constructor(op.getClass.getSimpleName))
    def apply (phase:Type, exp:Exp, op:Operation) : Constructor = {
      val crossP = exp.getClass.getSimpleName + op.getClass.getSimpleName
      'Op(phase, Constructor(crossP))
    }
    def apply (phase:Type, exp:Exp, ops:List[Operation]) : Constructor = {
      val crossP = exp.getClass.getSimpleName + ops.map(_.getClass.getSimpleName).mkString("")
      'Op(phase, Constructor(crossP))
    }
    def apply (phase:Type, ops:List[Operation]) : Constructor = {
      'Op(phase, Constructor(ops.map(_.getClass.getSimpleName).mkString("")))
    }

    val interface:Type       = 'Interface
    val defaultMethods:Type  = 'Default
    val finalType:Type       = 'Final
  }

}
