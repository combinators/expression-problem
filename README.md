# ExpressionProblem
The goal of this project is to generate a number of approaches (in multiple languages) that address the _Expression Problem_.

As coined by Philip Wadler [1], the Expression Problem is a new name for an old problem. The goal is to define a datatype by cases, where one can add new cases to the datatype and new functions over the datatype, without recompiling existing code, and while retaining static type safety.
 
There are various "solutions" to the Expression Problem. Each solution varies in the amount of code a user must write to implement them, and the language features they require.

In this project, we explore a number of such solutions. Our concern is not with the individual solutions to the Expression Problem (of which there are many), but rather the engineering of these. We provide an alternative, namely, to regenerate all code after modifying the domain.

## Installation

Once you have cloned this repository (branch `firstVersion`) you will need to make sure that you have a working JDK 1.8 installation, because this original version (from 2018) used libraries (the Play Framework) that required this original version.

On a Windows PC, for example, you would issue the following commands. Note that to eliminate spaces in the Path names, use the old DOS-style
option to replace "Program Files (x86)" with either progra~2, if the path
to your JDK installation contains spaces.

Once SBT is running, you can launch the language-specific server that will
be the host for different approaches in that language.

<pre><code>
<b style='color:#5FCA1C'>></b> set %JAVA_HOME%=c:\progra~2\java\jdk1.8.0_161
<b style='color:#5FCA1C'>></b> sbt
<b style='color:#5FCA1C'>sbt:expression-problem></b> compile
</code></pre>

This will properly compile all code 

For example, to initiate Java code generation, type:

<pre><code>
<b style='color:#5FCA1C'>sbt:expression-problem></b> language-java/run
[info] Updating core...
[info] Done updating.
[warn] There may be incompatibilities among your library dependencies; run 'evicted' to see detailed eviction warnings.
[info] Updating domain-math...
[info] Updating domain-shape...
[info] Done updating.
[warn] There may be incompatibilities among your library dependencies; run 'evicted' to see detailed eviction warnings.
[info] Done updating.
[warn] There may be incompatibilities among your library dependencies; run 'evicted' to see detailed eviction warnings.
[info] Updating language-java...
[info] Done updating.
[warn] There may be incompatibilities among your library dependencies; run 'evicted' to see detailed eviction warnings.
[info] Compiling 12 Scala sources to C:\YOUR-HOME-DIR\expression-problem\core\target\scala-2.12\classes ...
[info] Done compiling.
[info] Compiling 14 Scala sources to C:\YOUR-HOME-DIR\expression-problem\domain\math\target\scala-2.12\classes ...
[info] Compiling 3 Scala sources to C:\YOUR-HOME-DIR\expression-problem\domain\shape\target\scala-2.12\classes ...
[info] Done compiling.
[info] Done compiling.

--- (Running the application, auto-reloading is enabled) ---

[info] p.c.s.AkkaHttpServer - Listening for HTTP on /0:0:0:0:0:0:0:0:9000

(Server started, use Enter to stop and go back to the console...)
</code></pre>

At this point, you can now request different EP approaches to generate code in Java that contain 
implementations for the Math Domain.

## Math Domain

Let's start with a language for a simple form of arithmetic expressions. We first model the domain using Scala in [BaseDomain](src/main/scala/example/expression/domain/BaseDomain.scala):

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

For more details on [ModelDomain](core/src/main/scala/org/combinators/ep/domain/ModelDomain.scala) check out this Scala file. Once these concepts are identified, the designer chooses a programming language and implements a desired solution.

## Application Domain

The desired application domain (in this case mathematical expressions) 
extends these traits to provide a specific domain within which to work. 
The entire evolution history is modeled, from an initial state M0 through 
successive evolutions. 
The following [MathDomain](domain/math/src/main/scala/org/combinators/ep/domain/math/MathDomain.scala) 
describes the common domain used in the literature when describing the 
Expression Problem.

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

## OO Solution

A straight object-oriented approach requires operations to be added to each data type
class. As new subtypes are created, each can be placed in its own class and there is 
no trouble with existing code; however, defining new operations means that all existing
subtypes need to have new methods added to their class. As such, this is not a solution
to the EP problem.

To generate the code, start up a web browser and type the following 
as the URL address:

`http://localhost:9000/oo/m4`

The first time you issue this request, it may take up to 30 seconds to respond.
This requests the Java code generator to generate system `m4` using the `oo` EP approach.
In response, the SBT console will compile (if necessary) and produce output
showing that the Dev application has started:

```
[info] Compiling 56 Scala sources and 7 Java sources to C:\Users\heineman\IdeaProjects\expression-problem\language\java\target\scala-2.12\classes ...
[info] Done compiling.
DEBUG  - Generated dev mode secret ad38443e8cf8f6eddfa941b4049cbc97 for app at file:/C:/Users/heineman/IdeaProjects/expression-problem/language/java/target/scala-2.12/classes/application.conf
INFO   - Slf4jLogger started
DEBUG  - Starting application default Akka system: application
DEBUG application - logger log1-Slf4jLogger started
DEBUG application - Default Loggers started
INFO   - Enabled Filters (see <https://www.playframework.com/documentation/latest/Filters>):

    play.filters.csrf.CSRFFilter
    play.filters.headers.SecurityHeadersFilter
    play.filters.hosts.AllowedHostsFilter

INFO   - Application started (Dev)
```

Once SBT has compiled and executed, the browser will then change to a 
web page where you can 

Then in a separate console window, type the following git request:

`git clone localhost:9000/oo/m4/m4.git`

![Retrieve ZIP file](https://github.com/combinators/expression-problem/raw/master/oo.zip) with generated source files

## Visitor Solution

The Visitor Design Pattern is not an acceptable solution to the Expression Problem 
because defining new data variants (i.e., `Neg` which negates an expression) 
requires modifications to all existing `Visitor` classes. However, using our 
approach, we can simply resynthesize all classes with every change to the 
Application Domain. 

`git clone localhost:9000/scalaVisitor/m4/m4.git`

![Retrieve ZIP file](https://github.com/combinators/expression-problem/raw/master/scalaVisitor.zip) with generated source files

## Covariant Java Solution

The *Modularity 2016* paper [The Expression Problem, Trivially!](http://i.cs.hku.hk/~bruno/papers/Modularity2016.pdf "Expression Problem, Trivially!")
by *Yanling Wang* and *Bruno C. d. S. Oliveira* [2]
describes an approach using _covariant type refinement_ of return types and fields. Unlike existing solutions in
Java-like languages, this solution does not use any kind of generics.

`git clone localhost:9000/trivially/m4/m4.git`

![Retrieve ZIP file](https://github.com/combinators/expression-problem/raw/master/trivially.zip) with generated source files

## Interpreter Design Pattern

The *TCS 2003 paper* [Solving Expression problem using Interpreter Pattern](http://www.cs.pomona.edu/~kim/ftp/WOOD.pdf) by 
*Bruce Kim* [3] describes an approach to solving the EP problem using the Interpreter Design Pattern.

`git clone localhost:9000/interpreter/m4/m4.git`

![Retrieve ZIP file](https://github.com/combinators/expression-problem/raw/master/interpreter.zip) with generated source files

## Object Algebras

The *ECOOP 2012 paper* [Extensibility for the Masses](https://dl.acm.org/citation.cfm?id=236716) by 
*Bruno C. d. S. Oliveira & William R. Cook* [4] describes an approach to solving the EP problem using
Object Algebras.

`git clone localhost:9000/algebra/m4/m4.git`

![Retrieve ZIP file](https://github.com/combinators/expression-problem/raw/master/algebra.zip) with generated source files

## C++ Solution

C++ solutions exist as well. 
In a blog [Expression Problem and its solutions](https://eli.thegreenplace.net/2016/the-expression-problem-and-its-solutions "Expression Problem and its solutions")
Eli Bendersky outlines an approach for using the visitor design pattern as implemented in C++.

`git clone localhost:9000/cpp/m3/m3.git`

 This solution only generates for the first three evolutions. It compiles as follows:
 
`g++ *.cpp -Icpputest/include -Lcpputest/cpputest_build/lib -lCppUTest -lCppUTestExt`

The [CPPUnit test project](https://github.com/cpputest/cpputest) contains the necessary includes and libraries to 
cleanly compile this code and confirm all test cases.
 
 ![Retrieve ZIP file](https://github.com/combinators/expression-problem/raw/master/cpp.zip) with generated source files

 



# References

1. Wadler, Philip, [Email to to Java Genericity Mailing List](http://homepages.inf.ed.ac.uk/wadler/papers/expression/expression.txt)
2. Wang, Yanling and Bruno C. d. S. Oliveira, [The Expression Problem, Trivially!](https://dl.acm.org/citation.cfm?id=2889448), MODULARITY 2016, pp. 37-41.
3. Kim, Bruce, [Some Challenging Typing Issues in Object-Oriented Languages: Extended Abstract](http://doi.org/10.1016/S1571-0661(04)80799-0), TCS 82(8) 2003.
4. d. S. Oliveira, Bruno C. and William R. Cook [Extensibility for the Masses](https://dl.acm.org/citation.cfm?id=236716), ECOOP 2012
