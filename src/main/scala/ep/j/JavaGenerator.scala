package ep.j  /*DI:LD:AI*/

import com.github.javaparser.JavaParser
import com.github.javaparser.ast.body.{ConstructorDeclaration, FieldDeclaration, MethodDeclaration, TypeDeclaration}
import com.github.javaparser.ast.stmt.BlockStmt
import ep.domain.{BaseDomain, ModelDomain}
import ep.generator.{LanguageIndependentGenerator, Producer}
import org.combinators.templating.twirl.Java

import scala.collection.JavaConverters._

/**
  * Any Java-based EP approach can extend this Generator
  */
trait JavaGenerator extends LanguageIndependentGenerator  with Producer {
  val domain:BaseDomain with ModelDomain

  type CompilationUnit = com.github.javaparser.ast.CompilationUnit
  type Type = com.github.javaparser.ast.`type`.Type
  type Expression = com.github.javaparser.ast.expr.Expression
  type Statement = com.github.javaparser.ast.stmt.Statement
  type InstanceExpression = com.github.javaparser.ast.expr.Expression

  /** Return designated Java type associated with type, or void if all else fails. */
  override def typeConverter(tpe:domain.TypeRep) : Type = {
    tpe match {
      case domain.baseTypeRep => Java(s"${domain.baseTypeRep.name}").tpe()
      case _ => super.typeConverter(tpe)
    }
  }

  /** Convert a test instance into a Java Expression for instantiating that instance. */
  override def convert(ai: domain.Inst): Expression = {
    ai match {
      case ui: domain.UnaryInst =>
        inst(ui.e, convert(ui.inner))
      case bi: domain.BinaryInst =>
        inst(bi.e, convert(bi.left), convert(bi.right))
      case ai:domain.AtomicInst =>
        inst(ai.e, instConverter(ai.ei))

      case _ => Java(s""" "unknown ${ai.toString}" """).expression()
    }
  }

  /**
    * Return expression associated with instance.
    *
    * Handles the top-level
    */
  override def instConverter(ei:domain.ExistsInstance) : Expression = {
    ei.inst match {
      case di:domain.Inst => convert (di)
      case _ => super.instConverter(ei)
    }
  }

  /**
    * Default behavior in Java is to return an expression value.
    */
  def result (expr:Expression) : Seq[Statement] = {
    Java(s"return $expr;").statements()
  }

  /**
    * For producer operations, there is a need to instantiate objects, and one would use this
    * method (with specific parameters) to carry this out.
    */
  def inst(exp:domain.DataType, params:Expression*): Expression = {
    Java("new " + exp.concept + "(" + params.map(expr => expr.toString).mkString(",") + ")").expression[InstanceExpression]()
  }

  // Useful helper methods for any generator needing to craft common Java constructs

  /** Generate constructor for given atomic concept, using suggested name */
  def constructor(exp:domain.DataType, suggestedName:Option[String] = None) : ConstructorDeclaration = {
    val name = if (suggestedName.isEmpty) { exp.name } else { suggestedName.get }

    val params:Seq[String] = exp.attributes.map(att => s"${typeConverter(att.tpe)} ${att.instance}")
    val cons:Seq[Statement] = exp.attributes.flatMap(att => Java(s"  this.${att.instance} = ${att.instance};").statements())

    val str =  s"""|public $name (${params.mkString(",")}) {
                   |   ${cons.mkString("\n")}
                   |}""".stripMargin
    Java(str).constructors().head
  }

  /** Generate constructor for given operation, using suggested name */
  def constructorFromOp(op:domain.Operation, suggestedName:Option[String] = None) : ConstructorDeclaration = {
    val name = if (suggestedName.isEmpty) { op.concept } else { suggestedName.get }

    val params:Seq[String] = op.parameters.map(param => s"${typeConverter(param.tpe)} ${param.name}")
    val cons:Seq[Statement] = op.parameters.flatMap(param => Java(s"  this.${param.name} = ${param.name};").statements())

    val str = s"""|public $name (${params.mkString(",")}) {
                  |   ${cons.mkString("\n")}
                  |}""".stripMargin
    println ("STR2:" + str)
    Java(str).constructors().head
  }

  /** Compute parameter "name" comma-separated list from operation. */
  def arguments(op:domain.Operation) : String = {
    op.parameters.map(param => param.name).mkString(",")
  }

  /** Compute parameter "Type name" comma-separated list from operation. */
  def parameters(op:domain.Operation) : String = {
    op.parameters.map(param => typeConverter(param.tpe).toString + " " + param.name).mkString(",")
  }

  /**
    * Produce all getter methods for the given exp, with suitable possibility of using covariant replacement
    * on domain.BaseTypeRep
    */
  def getters(exp:domain.DataType) : Seq[MethodDeclaration] =

    exp.attributes.flatMap(att => Java(s"""|public ${typeConverter(att.tpe)} get${att.concept}() {
                                           |    return this.${att.instance};
                                           |}""".stripMargin).methodDeclarations)

  /**
    * Given an exp, produce a sequence of field declarations, for each of the attributes.
    *
    * @param exp
    * @return
    */
  def fields(exp:domain.DataType) : Seq[FieldDeclaration] = {
    exp.attributes.flatMap(att => Java(s"private ${typeConverter(att.tpe)} ${att.instance};").fieldDeclarations())
  }

  /**
    * Return a new class with designated superclass and potential implements clauses
    * @param pkgName      Name of package into which class belongs (if "" then default package)
    * @param className    Desired class name
    * @param implements   sequence of strings representing interfaces
    * @param superclass   potential superclass
    * @return
    */
  def makeClass(pkgName:String, className:String, implements:Seq[String] = Seq.empty, superclass:Option[String] = None) : CompilationUnit = {
    val packageClause = if (pkgName == "") {
      ""
    } else {
      s"package $pkgName;"
    }

    val implementsClause:String = if (implements.isEmpty) {
      ""
    } else {
      "implements " + implements.mkString(",")
    }

    val extendsClause:String = if (superclass.isEmpty) {
      ""
    } else {
      "extends " + superclass.get
    }
    Java(s"""|$packageClause
             |public class $className $extendsClause $implementsClause {
             |
             |}""".stripMargin).compilationUnit
  }

  /**
    * Return a new class with designated superclass and potential implements clauses
    * @param pkgName      Name of package into which class belongs (if "" then default package)
    * @param className    Desired class name
    * @param implements   sequence of strings representing interfaces
    * @param parentInterface   potential superclass
    * @return
    */
  def makeInterface(pkgName:String, className:String, implements:Seq[String] = Seq.empty, parentInterface:Option[String] = None) : CompilationUnit = {
    val packageClause = if (pkgName == "") {
      ""
    } else {
      s"package $pkgName;"
    }

    val implementsClause:String = if (implements.isEmpty) {
      ""
    } else {
      "implements " + implements.mkString(",")
    }

    val extendsClause:String = if (parentInterface.isEmpty) {
      ""
    } else {
      "extends " + parentInterface.get
    }
    Java(s"""|$packageClause
             |public interface $className $extendsClause $implementsClause {
             |
             |}""".stripMargin).compilationUnit
  }

  /**
    * Add methods to the class.
    *
    * No check is made to determine whether duplicate methods would exist.
    *
    * @param unit       Class to be modified
    * @param methods    Methods to be added.
    * @return
    */
  def addMethods(unit:CompilationUnit, methods:Seq[MethodDeclaration]) : CompilationUnit = {
    val clazz = unit.getType(0)
    methods.foreach(m => clazz.addMember(m))

    unit
  }

  /**
    * Add Javadoc-style comment to primary type of CompilationUnit
    *
    * @param unit       CompilationUnit to be modified
    * @param doc        Comment to be applied as javadoc to the given type
    * @param typeIndex  Which type (defaults to 0) to have javadoc modified.
    * @return
    */
  def addTypeComment(unit:CompilationUnit, doc:String, typeIndex:Int = 0) : CompilationUnit = {
    unit.getType(typeIndex).setComment(JavaParser.parseJavadoc(doc).toComment(""))
    unit
  }


  def copyDeclarations (oldType:TypeDeclaration[_], newType:TypeDeclaration[_]) : Unit = {
    val elements = oldType.getMembers.iterator().asScala
    while (elements.hasNext) {
      newType.addMember(elements.next)
    }
  }

  /**
    * Add statements to the end of the given method.
    *
    * @param method
    * @param stmts
    * @return
    */
  def addStatements(method:MethodDeclaration, stmts:Seq[Statement]) : MethodDeclaration = {
    if (!method.getBody.isPresent) {
      val bb:BlockStmt = new BlockStmt()
      method.setBody(bb)
    }
    var block = method.getBody.get

    stmts.foreach(s => block = block.addStatement(s))
    method.setBody(block)
    method
  }
}
