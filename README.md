# ExpressionProblem
The goal of this project is to generate a number of approaches (in multiple languages) that address the _Expression Problem_.

As coined by Philip Wadler [1], the Expression Problem is a new name for an old problem. The goal is to define a datatype by cases, where one can add new cases to the datatype and new functions over the datatype, without recompiling existing code, and while retaining static type safety.
 
There are various "solutions" to the Expression Problem. Each solution varies in the amount of code a user must write to implement them, and the language features they require.

In this project, we explore a number of such solutions. Our concern is not with the individual solutions to the Expression Problem (of which there are many), but rather the engineering of these. We provide an alternative, namely, to regenerate all code after modifying the domain.


## Installation

Once you have cloned this repository (branch `originalPrototype`) you will need to make sure
that you have a working Scala Built Tool (SBT) installation. You should have a working JDK
installation that is `1.8` and `11`; it should also work with JDK `17`.

On a Windows PC, for example, you would issue the following commands. Note that to eliminate spaces in the Path names, use the old DOS-style
option to replace "Program Files (x86)" with either progra~2 or progra~1, if the path
to your JDK installation contains spaces.

Once SBT is running, you can launch the language-specific server that will
be the host for different approaches in that language.

<pre><code>
<b style='color:#5FCA1C'>></b> set %JAVA_HOME%=c:\progra~2\java\jdk1.8.0_161
<b style='color:#5FCA1C'>></b> sbt
<b style='color:#5FCA1C'>sbt:expression-problem></b> compile
</code></pre>

Alternative, you could launch `sbt` with the command line argument to choose the desired JVM to use:

<pre><code>
<b style='color:#5FCA1C'>></b> sbt --java-home="C:/Progra~2/Java/jdk1.8.0_161/"
<b style='color:#5FCA1C'>sbt:expression-problem></b> compile
</code></pre>

This will properly compile all code

At this point, you can now request different EP approaches to generate code in Java that contain
implementations for the Math Domain.

Let's start with a language for a simple form of arithmetic expressions. We first model the domain using Scala in BaseDomain:

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
  case class Attribute(name:String, tpe:TypeRep) extends Element
  abstract class Operation(val name:String, val returnType:Option[TypeRep], val parameters:Seq[(String, TypeRep)] = Seq.empty) extends Element

  abstract class Atomic(val name: String, val attributes: Seq[Attribute])
  abstract class Unary(override val name:String) extends Atomic(name, Seq(Attribute(base.inner, baseTypeRep)))
  abstract class Binary(override val name:String) extends Atomic(name, Seq(Attribute(base.left, baseTypeRep), Attribute(base.right, baseTypeRep)))

  class ProducerOperation(override val name:String, override val parameters:Seq[(String, TypeRep)]) extends Operation(name,Some(baseTypeRep),parameters)
  class BinaryMethod(override val name:String, override val returnType:Option[TypeRep]) extends Operation(name, returnType, Seq((base.that, baseTypeRep)))

  class AtomicInst(val e:Atomic, val i:Option[Any]) 
  class UnaryInst(override val e:Atomic, val inner:AtomicInst) extends AtomicInst(e, None)
  class BinaryInst(override val e:Atomic, val left:AtomicInst, val right:AtomicInst) extends AtomicInst(e, None)
}

trait ModelDomain extends BaseDomain {
  case class Model(name:String,          
    types:Seq[Atomic], ops:Seq[Operation],
    last:Model = emptyModel()) {
        ...    // implementation omitted
    }                                    
}

```

For more details on `ModelDomain` check out this Scala file. Once these concepts are identified, the designer chooses a programming language and implements a desired solution.

## Application Domain

The desired application domain (in this case mathematical expressions) extends these traits to provide a specific domain within which to work. The entire evolution history is modeled, from an initial state M0 through successive evolutions. The following `MathDomain` describes the common domain used in the literature when describing the Expression Problem.

```
trait MathDomain extends BaseDomain with ModelDomain {  
  case object Exp extends TypeRep {
    override def name:String = "Exp"
  }
  type BaseTypeRep = Exp.type
  val baseTypeRep:BaseTypeRep = Exp
}                                                      
object MathDomain extends MathDomain

trait Evolution {                                
  val domain: ModelDomain
  def getModel: domain.Model
}                                                

trait M0 extends Evolution {                      
  val domain:MathDomain
  import domain._
  val litValue:String = "value"

  case object Double extends TypeRep
  case object Lit extends Atomic("Lit", Seq(Attribute(litValue, Double)))
  case object Add extends Binary("Add")

  case object Eval extends Operation("eval", Some(Double)) 
  class LitInst(d:Double) extends AtomicInst(Lit, Some(d))
  val m0 = Model("m0", Seq(Lit, Add), Seq(Eval))
  override def getModel = m0
}                                                 

trait M1 extends Evolution { self: M0 =>         
  val domain:MathDomain
  case object Sub extends Binary("Sub")
  val m1 = Model("m1", Seq(Sub), Seq.empty, last=m0)  
  override def getModel = m1
}                                                 

trait M2 extends Evolution { self: M0 with M1 =>  
  val domain:MathDomain
  import domain._
  case object String extends TypeRep
  case object PrettyP extends Operation("print",Some(String)) 
  val m2 = Model("m2", Seq.empty, Seq(PrettyP), last=m1)
  override def getModel = m2
}          

trait M3 extends Evolution { self: M0 with M1 with M2 =>
  val domain:MathDomain

  case object Mult extends domain.Binary("Mult")
  case object Neg extends domain.Unary("Neg")
  case object Divd extends domain.Binary("Divd")

  val m3 = domain.Model("m3", Seq(Neg, Mult, Divd), Seq.empty, last = m2)
  override def getModel = m3
}

trait M4 extends Evolution {
  self: M0 with M1 with M2 with M3 =>
  val domain:MathDomain

  case object Simplify extends domain.ProducerOperation("simplify")
  case class List(generic:domain.TypeRep) extends domain.TypeRep
  case object Collect extends domain.Operation("collect", Some(List(Double)))

  val m4 = domain.Model("m4",Seq.empty, Seq(Simplify, Collect), last = m3)
  override def getModel = m4
}

```

In this application domain, an initial model (M0) is extended four times, adding new data
types and operations. We have encoded a number of approaches to the Expression Problem that 
generates solutions in Java. To request the code generation, the following are the completed 
implementations

# Java Solutions

We encoded several EP approaches that generate Java code for both `MathDomain` and `ShapeDomain`.

For these Java EP approaches, we can generate up to eight systems for `MathDomain` (e1 through e6)
and for the `ShapeDomain` we can generate (s0, s1).

To generate the code for system M3, type the following in SBT:

<pre><code>
<b style='color:#5FCA1C'>sbt:expression-problem></b> runMain ep.j.GenerateAll
</code></pre>


# Haskell Solutions

We generate a number of solutions in Haskell, which can be generated as follows:

<pre><code>
<b style='color:#5FCA1C'>sbt:expression-problem></b> runMain ep.haskell.GenerateAll
</code></pre>


## Trees that grow

The JUCS 2017 paper [Trees that Grow](https://lib.jucs.org/article/22912/list/9/) by _Najd & Jones_ describes a programming idiom that exploits type-level functions to allow a particular form of extensibility.

This solution only generates for the first three evolutions.

## Data Types A La Carte

The 2008 paper [Data Types à la carte](https://doi.org/10.1017/S0956796808006758) by
Swierstra describes a technique for assembling both data types and functions from isolated individual components.

# GJ Solutions

We can generate gj code using the Wadler approach.

To generate the code for system M1, type the following in SBT:

<pre><code>
<b style='color:#5FCA1C'>sbt:expression-problem></b> runMain ep.gj.GenerateAll
</code></pre>

This solution only generates for the first two evolutions, and there is no gj compiler available to validate it.

# C++ Solutions

We can generate several C++ solutions.

<pre><code>
<b style='color:#5FCA1C'>sbt:expression-problem></b> runMain ep.cpp.GenerateAll
</code></pre>

# References

1. Wadler, Philip, [Email to to Java Genericity Mailing List](http://homepages.inf.ed.ac.uk/wadler/papers/expression/expression.txt)
2. Wang, Yanling and Bruno C. d. S. Oliveira, [The Expression Problem, Trivially!](https://dl.acm.org/citation.cfm?id=2889448), MODULARITY 2016, pp. 37-41.
3. Kim, Bruce, [Some Challenging Typing Issues in Object-Oriented Languages: Extended Abstract](http://doi.org/10.1016/S1571-0661(04)80799-0), TCS 82(8) 2003.
4. d. S. Oliveira, Bruno C. and William R. Cook [Extensibility for the Masses](https://dl.acm.org/citation.cfm?id=236716), ECOOP 2012
