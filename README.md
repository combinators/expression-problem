# ExpressionProblem
Synthesize a number of approaches (in multiple languages) that address the _Expression Problem_.

As coined by Philip Walder, the Expression Problem is a new name for an old problem. The goal is to define a datatype by cases, where one can add new cases to the datatype and new functions over the datatype, without recompiling existing code, and while retaining static type safety.
 
There are various "solutions" to the Expression Problem. Each solution varies in the amount of code a user must write to implement them, and the language features they require.

In this project, we explore a number of such solutions. Our concern is not with the individual solutions to the Expression Problem (of which there are many), but rather the engineering of these. We provide an alternative, namely, to regenerate all code after modifying the domain.

Let's start with a language for a simple form of arithmetic expressions. We first model the domain using Scala:

```
trait BaseDomain {
  abstract class TypeRep { 
    def name: String = getClass.getName
  }
  type BaseTypeRep <: TypeRep
  val baseTypeRep:BaseTypeRep  

  object base {
    val inner:String = "inner"
    val left:String  = "left"
    val right:String = "right"
    val that:String  = "that"
  }

  abstract class Element
  case class Attribute(name:String, tpe:TypeRep)
    extends Element
  abstract class Operation(val name:String,
    val returnType:Option[TypeRep],
    val parameters:Seq[(String, TypeRep)] = Seq.empty)
    extends Element

  abstract class Atomic(val name: String,   
    val attributes: Seq[Attribute])
  abstract class Unary(override val name:String)
    extends Atomic(name,
      Seq(Attribute(base.inner, baseTypeRep)))
  abstract class Binary(override val name:String)
    extends Atomic(name,
      Seq(Attribute(base.left, baseTypeRep),
          Attribute(base.right, baseTypeRep)))    

 class ProducerOperation(override val name:String,  
    override val parameters:Seq[(String, TypeRep)])
    extends Operation(name,Some(baseTypeRep),parameters)  
  class BinaryMethod(override val name:String,  
    override val returnType:Option[TypeRep])
    extends Operation(name, returnType,
    Seq((base.that, baseTypeRep)))            


  class AtomicInst(val e:Atomic, val i:Option[Any]) 
  class UnaryInst(override val e:Atomic,
    val inner:AtomicInst) extends AtomicInst(e, None)
  class BinaryInst(override val e:Atomic,
    val left:AtomicInst, val right:AtomicInst)
    extends AtomicInst(e, None) |\label{line:inst-end}|
}

trait ModelDomain extends BaseDomain {
  case class Model(name:String,          
    types:Seq[Atomic], ops:Seq[Operation],
    last:Model = emptyModel()) {
        ...    // implementation omitted
    }                                    
}

```

Once these concepts are identified, the designer chooses a programming language and implements a desired solution.

## Application Domain

![UML Domain Model](https://github.com/combinators/expression-problem/blob/master/ExpressionProblemModel.png)

## Visitor Solutions

The Visitor Design Pattern is not an acceptable solution to the Expression Problem 
because defining new data variants (i.e., `Neg` which negates an expression) 
requires modifications to all existing `Visitor` classes. However, using our 
approach, we can simply resynthesize all classes with every change to the 
Application Domain. 

**inhabit:** `localhost:9000/expression`

## Covariant Java Solution

The *Modularity 2016* paper [The Expression Problem, Trivially!](http://i.cs.hku.hk/~bruno/papers/Modularity2016.pdf "Expression Problem, Trivially!")
by *Yanling Wang* and *Bruno C. d. S. Oliveira*
describes an approach using _covariant type refinement_ of return types and fields. Unlike existing solutions in
Java-like languages, this solution does not use any kind of generics.

**inhabit:** `localhost:9000/ep`

## C++ Solution

C++ solutions exist as well. 
In a blog [Expression Problem and its solutions](https://eli.thegreenplace.net/2016/the-expression-problem-and-its-solutions "Expression Problem and its solutions")
Eli Bendersky outlines an approach for using the visitor design pattern as implemented in C++.

**inhabit:** `localhost:9000/cpp_ep`

 
 



