Papers on extensibility and language design

original paper that led to the Krsihnamurthi 1997 ECOOP example
http://www.cs.yale.edu/publications/techreports/tr1049.pdf

ECOOP paper by Krishnamurthi
https://users.wpi.edu/~heineman/tmp/paper.pdf

Expression Problem Trivially
http://i.cs.hku.hk/~bruno/papers/Modularity2016.pdf

Family Polymorphism
http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.646.5270&rep=rep1&type=pdf

Full code examples here:
https://eli.thegreenplace.net/2016/the-expression-problem-and-its-solutions/

Note. Conduct small experiment within Expression problem. Then (based on sample code above)
synthesize multiple different solutions from the same domain model. Think matrix for
specifying the variations (both in op and in type).

http://i.cs.hku.hk/~bruno/oa/ contains other full solutions

val l1 = new Lit{val x=4}
val l2 = new Lit{val x=3}
val a = new Add{val e1=l1; val e2=l2}
println ("a.eval = " + a.eval)

val s = new Sub{val e1=l1; val e2=l2}
println ("s.eval = " + s.eval)

// here's how to synthesize AddP
val le1 = new LitP{val x=4}
val le2 = new LitP{val x=3}
val ae = new AddP{val e1=le1; val e2 = le2}
println (ae.print + " = " + ae.eval)

Synthesis target: give an Expression that supports X,Y,Z

Add needs two expressions

Produced code:


------------------------
package expression;
public class PrettyP extends Visitor<String> {
  public String visit(Lit e) { return "" + e.value(); }
  public String visit(Add e) { return "(" +
    e.left().accept(this) + "+" + e.right().accept(this) + ")";
  }
  public String visit(Sub e) { return "(" +
    e.left().accept(this) + "-" + e.right().accept(this) + ")";
  }
}

------------------------

package ep;
interface LitPrettyP extends Lit, PrettyP {
  default String print() { return "" + value(); }
}
interface AddPrettyP extends Add, PrettyP {
  PrettyP left();
  PrettyP right();
  default String print() { return "(" +
    left().print() + " + " + right().print() + ")";
  }
}
interface SubPrettyP extends Sub, PrettyP {
  PrettyP left();
  PrettyP right();

  default String print() { return "(" +
    left().print() + " - " + right().print() + ")";
  }
}
