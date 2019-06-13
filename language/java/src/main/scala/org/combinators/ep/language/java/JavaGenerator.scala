package org.combinators.ep.language.java   /*DI:LD:AI*/

import com.github.javaparser.JavaParser
import com.github.javaparser.ast.body.{ConstructorDeclaration, FieldDeclaration, MethodDeclaration, TypeDeclaration}
import com.github.javaparser.ast.stmt.BlockStmt
import org.combinators.ep.domain.{BaseDomain, ModelDomain}
import org.combinators.ep.generator.LanguageIndependentGenerator
import org.combinators.templating.twirl.Java

import scala.collection.JavaConverters._

/**
  * Any Java-based EP approach can extend this Generator.
  *
  * @groupname lang Language Bindings
  * @groupdesc lang Fundamental Language Bindings as required by EpCoGen framework
  * @groupprio lang 5
  *
  * @groupname api Core API
  * @groupdesc api Fundamental abstractions to provide context for [[LanguageIndependentGenerator]]
  * @groupname api Core API
  * @groupname deltaHelpers DeltaHelpers
  * @groupname context Context
  * @groupname types Parameterized Types
  * @groupname dependency External Dependencies
  * @groupname inst Instantiating data types
  * @groupdesc dependency Depends upon BaseDomain (for the core logic) and the desired
  * @groupdesc api Fundamental abstractions needed for any language-based solution to EP
  * @groupprio api 0
  * @groupdesc types Each language must define relevant abstractions that map to these types.
  *            It is acceptable if the same structure is used for multiple types (as an example,
  *            review CPPElement)
  * @groupprio types 10
  * @groupdesc context Each language and approach needs different solutions to assemble the logic
  *           for a given (data-type and operation). The top-level concepts are shown here.
  * @groupprio types 20
  * @groupdesc inst When generating test cases, it is essential to include construction
  *            code that constructs instances of the data types. In addition, some receursive
  *            operations depend on being able to constrct instances of data types.
  * @groupprio context 30
  * @groupdesc deltaHelpers When weaving together code expressions representing partial fragments
  *           for a given logic, these helper methods are useful in capturing the desired structures.
  * @groupprio deltaHelpers 40
  */
trait JavaGenerator extends LanguageIndependentGenerator {
  val domain:BaseDomain with ModelDomain

  /** @group lang */
  type CompilationUnit = com.github.javaparser.ast.CompilationUnit

  /** @group lang */
  type Type = com.github.javaparser.ast.`type`.Type

  /** @group lang */
  type Expression = com.github.javaparser.ast.expr.Expression

  /** @group lang */
  type Statement = com.github.javaparser.ast.stmt.Statement

  /**
    * Return designated Java type associated with type.
    * @group api
    */
  override def typeConverter(tpe:domain.TypeRep) : Type = {
    tpe match {
      case domain.baseTypeRep => Java(s"${domain.baseTypeRep.name}").tpe()
      case _ => super.typeConverter(tpe)
    }
  }

  /**
    * In Java, an expression must be returned by the 'return' statement.
    * @group api
    */
  def result (expr:Expression) : Seq[Statement] = {
    Java(s"return $expr;").statements()
  }

  /**
    * For producer operations, there is a need to instantiate objects, and one would use this
    * method (with specific parameters) to carry this out.
    */
  def inst(exp:domain.DataType, params:Expression*): CodeBlockWithResultingExpressions = {
    CodeBlockWithResultingExpressions(
      Java(s"new ${exp.concept}${params.mkString("(", ", ", ")")}").expression()
    )
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

    Java(s"""|public $name (${params.mkString(",")}) {
                  |   ${cons.mkString("\n")}
                  |}""".stripMargin).constructors().head
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
  def appendStatements(method:MethodDeclaration, stmts:Seq[Statement]) : MethodDeclaration = {
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
