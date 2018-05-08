package example.expression.covariant

import org.combinators.cls.types.{Constructor, Type}
import org.combinators.cls.types.syntax._
import expression.{Exp, Operation}


/**
  * These codify the semantic types used by the Expression problem.
  *
  * For any of these that are ever going to be translated directly into Java Type Names, you must
  * make them Constructor.
  */
trait SemanticTypes {

  val driver:Type = 'Driver

  // meta-concerns. When you have completed the definition of a constructor
  object generated {
    def apply (uniq:Type) : Constructor = 'Generated(uniq)

    val visitor: Type = 'Visitor
    val complete: Type = 'Complete
    val initialized: Type = 'Initialized
  }

  object exp {
    def apply (phase:Type, exp:Exp) : Constructor =  'Exp(phase, Constructor(exp.getClass.getSimpleName))

    val base:Type = 'Base           // initial class
    val visitor:Type = 'Visitor     // once visitor has been added

  }

  /**
    * Each operation has its own type, assuming operation names are valid Java SimpleNames.
    */
  object ops {
    def apply (phase:Type, op:Operation) : Constructor = 'Ops(phase, Constructor(op.getClass.getSimpleName))

    val base:Type = 'Base           // initial class
    val visitor:Type = 'Visitor     // once visitor has been added
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
      val crossP = exp.getClass.getSimpleName + ops.map(_.getClass.getSimpleName).sortWith(_ < _).mkString("")
      'Op(phase, Constructor(crossP))
    }
    def apply (phase:Type, ops:List[Operation]) : Constructor = {
      'Op(phase, Constructor(ops.map(_.getClass.getSimpleName).sortWith(_ < _).mkString("")))
    }

    val interface:Type       = 'Interface
    val defaultMethods:Type  = 'Default
    val finalType:Type       = 'Final
  }

  // For example, PrettyPrint + Collect -> architecture(PrettyP, Collect)
//  object architecture {
//    def apply (one:Exp, two:Exp)           : Constructor = 'Arch(Constructor(one.getClass.getSimpleName), Constructor(two.getClass.getSimpleName))
//    def apply (head:Exp, tail:Constructor) : Constructor = 'Arch(Constructor(head.getClass.getSimpleName), tail)
//  }

}
