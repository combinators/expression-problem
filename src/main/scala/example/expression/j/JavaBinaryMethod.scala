package example.expression.j

import com.github.javaparser.ast.`type`.Type
import com.github.javaparser.ast.body.{BodyDeclaration, MethodDeclaration}
import com.github.javaparser.ast.stmt.Statement
import example.expression.domain.{BaseDomain, ModelDomain}
import example.expression.generator.BinaryMethod
import org.combinators.templating.twirl.Java

import scala.collection.JavaConverters._

trait JavaBinaryMethod extends BinaryMethod {
  val domain:BaseDomain with ModelDomain
  import domain._

  type Declaration = BodyDeclaration[_]

  /**
    * Declares the helper classes needed.
    * @return
    */
  def declarations: Seq[Declaration] = {
    Java(
      s"""|class Wrapper {
          |	interface Subtypes {
          |		// empty by default. Extended
          |	};
          |
          |	public interface Tree {
          |		default java.util.Optional<Leaf> asLeaf() { return java.util.Optional.empty(); }
          |		default java.util.Optional<Node> asNode() {	return java.util.Optional.empty(); }
          |
          |		default boolean same (Tree o) {
          |			java.util.Optional<Boolean> leafCheck = this.asLeaf().flatMap(leaf -> o.asLeaf().map(leaf2 -> Boolean.valueOf(leaf.value.equals(leaf2.value))));
          |			java.util.Optional<Boolean> nodeCheck = this.asNode().flatMap(node -> o.asNode()
          |					.map(node2 -> {
          |						if (!node2.label.equals(node.label)) { return false; }    // must be same label
          |						if (node2.subtrees.size() != node.subtrees.size()) { return false; }  // short-circuit if not same length
          |
          |						java.util.Iterator<Tree> it1 = node.subtrees.iterator();   // all children must match.
          |						java.util.Iterator<Tree> it2 = node2.subtrees.iterator();
          |
          |						while (it1.hasNext() && it2.hasNext()) {
          |							if (!it1.next().same(it2.next())) { return false; }
          |						}
          |
          |						return true;
          |					}));
          |
          |			// only two possibilities, else false
          |			return (leafCheck.orElse(nodeCheck.orElse(false)));
          |		}
          | }
          |
          | class Node implements Tree {
          |		public final Subtypes label;
          |		java.util.List<Tree> subtrees = new java.util.ArrayList<Tree>();
          |
          |		public Node(java.util.List<Tree> children, Subtypes label) {
          |			this.label = label;
          |			subtrees.addAll(children);
          |		}
          |
          |		public java.util.Optional<Node> asNode() { return java.util.Optional.of(this); }
          | }
          |
          | class Leaf implements Tree {
          |		public final Object value;
          |
          |		public Leaf(Object e) {
          |			value = e;
          |		}
          |
          |		public java.util.Optional<Leaf> asLeaf() { return java.util.Optional.of(this); }
          | }
          |}
          """.stripMargin).compilationUnit().getType(0).getMembers.iterator.asScala.toSeq
  }

  /**
    * Compute parameter "Type name" comma-separated list from operation. Be sure to convert BaseType into op.name!
    *
    * @param op               operation under consideration
    * @param typeConverter    existing typeconverter which we need for other types besides baseTypeRep
    * @return                 return new parameter type with op interface used in place of baseTypeRep
    */
  def binaryMethodParameters(op:domain.Operation, typeConverter:(domain.TypeRep,Option[Type]) => Type) : String = {
    op.parameters.map(tuple => {
      val name:String = tuple._1
      val tpe:domain.TypeRep = tuple._2

      // use operation name for binary method
      val realType = tpe match {
        case domain.baseTypeRep => op.name.capitalize
        case _ => typeConverter(tpe, Option.empty)
      }

      realType.toString + " " + name
    }).mkString(",")
  }

  /**
    * Add defined data types for given exp subtype
    * @param context
    * @param exps
    */
  def definedDataSubTypes(context:String, exps:Seq[domain.Atomic]) :Seq[BodyDeclaration[_]] = {
    val realContext = if (context.equals("")) {
      ""
    } else {
      context + "."
    }
   Java (s"""
             |enum DefinedSubtypes implements ${realContext}Subtypes {
             |		${exps.map(exp => exp.name).mkString(",")}
             |}
           """.stripMargin).classBodyDeclarations()
    }

  def logicAsTree(exp:domain.Atomic) : Seq[MethodDeclaration] = {
    val args = exp.attributes.map(att => att.name).mkString(",")
          Java(
            s"""
               |public Tree ${domain.AsTree.name.toLowerCase}() {
               |  return asTree.${exp.name.toLowerCase}($args).${domain.AsTree.name.toLowerCase}();
               |}""".stripMargin).methodDeclarations()
  }

  /** Interesting shift needed for visitor. */
  def visitorLogicAsTree(exp:domain.Atomic) : Seq[MethodDeclaration] = {
    val atomicArgs = exp.attributes.map(att => att.name).mkString(",")

    // changes whether attributes can be access *directly* or whether they are accessed via getXXX*() method.
    val recursiveArgs = exp.attributes.map(att => att.name + s".${AsTree.name.toLowerCase}()").mkString(",")

    val body:Seq[Statement] = exp match {
      case b:Binary => {
        Java(s""" return new Node(java.util.Arrays.asList($recursiveArgs), DefinedSubtypes.${exp.name.capitalize}); """).statements
      }
      case u:Unary => {
        Java(s""" return new Node(java.util.Arrays.asList($recursiveArgs), DefinedSubtypes.${exp.name.capitalize}); """).statements
      }
      case a:Atomic => {
        Java(s""" return new Leaf($atomicArgs);""").statements
      }
    }


    Java(
      s"""
         |public Tree ${domain.AsTree.name.toLowerCase}() {
         |  ${body.mkString("\n")}
         |}""".stripMargin).methodDeclarations()
  }

}
