package ep.gj    /*DI:LD:AI*/

import ep.domain.ModelDomain
import org.combinators.ep.domain.{BaseDomain, ModelDomain}
import org.combinators.ep.generator.LanguageIndependentGenerator

/**
  * Any Java-based EP approach can extend this Generator
  */
trait GJGenerator extends LanguageIndependentGenerator {
  val domain:BaseDomain with ModelDomain

  type CompilationUnit = GJWithPath
  type Type = GJType
  type Expression = GJ
  type Statement = GJ

  /**
    * Default behavior in GJ is to return an expression value.
    */
  def result (expr:Expression) : Seq[Statement] = {
    Seq(GJ(s"return $expr;"))
  }

  // TODO: FIX
  override def contextDispatch(source:Context, delta:Delta) : Expression = {
    new GJ(s""""replaceMe"""")
  }

  // Useful helper methods for any generator needing to craft common Java constructs

  /** Generate constructor for given atomic concept, using suggested name */
  def constructor(exp:domain.Atomic, suggestedName:Option[String] = None) : GJ = {
    val name = if (suggestedName.isEmpty) { exp.name } else { suggestedName.get }

    val params:Seq[String] = exp.attributes.map(att => s"${typeConverter(att.tpe)} ${att.instance}")
    val cons:Seq[GJ] = exp.attributes.map(att => GJ(s"  ${att.instance}_ = ${att.instance};"))

    GJ(s"""|public $name (${params.mkString(",")}) {
             |   ${cons.mkString("\n")}
             |}""".stripMargin)
  }

  /** Compute parameter "name" comma-separated list from operation. */
  def arguments(op:domain.Operation) : String = {
    op.parameters.map(param => param.name).mkString(",")
  }

  /** Compute parameter "Type name" comma-separated list from operation. */
  def parameters(op:domain.Operation) : String = {
    op.parameters.map(param => typeConverter(param.tpe).toString + " " + param.name).mkString(",")
  }

  /** Compute parameter "Type name" comma-separated list from operation. */
  def parametersExp(exp:domain.Atomic) : String = {
    exp.attributes.map(att => typeConverter(att.tpe).toString + " " + att.instance).mkString(",")
  }

  /**
    * Produce all getter methods for the given exp, with suitable possibiity of using covariant replacement
    * on domain.BaseTypeRep
    */
  def getters(exp:domain.Atomic) : Seq[GJ] =

    exp.attributes.map(att => GJ(s"""|public ${typeConverter(att.tpe)} get${att.concept}() {
                                           |    return this.${att.instance};
                                           |}""".stripMargin))

  /**
    * Given an exp, produce a sequence of field declarations, for each of the attributes.
    * Note all fields to a class have _ as a suffix.
    *
    * @param exp
    * @return
    */
  def fields(exp:domain.Atomic) : Seq[GJ] = {
    exp.attributes.map(att => GJ(s"protected final ${typeConverter(att.tpe)} ${att.name}_;"))
  }

  /**
    * Return a new class with designated superclass and potential implements clauses
    * @param pkgName      Name of package into which class belongs (if "" then default package)
    * @param className    Desired class name
    * @param implements   sequence of strings representing interfaces
    * @param superclass   potential superclass
    * @return
    */
  def makeClass(pkgName:String, className:String, implements:Seq[String] = Seq.empty, superclass:Option[String] = None) : GJ = {
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
    GJ(s"""|$packageClause
             |public class $className $extendsClause $implementsClause {
             |
             |}""".stripMargin)
  }

  /**
    * Return a new class with designated superclass and potential implements clauses
    * @param pkgName      Name of package into which class belongs (if "" then default package)
    * @param className    Desired class name
    * @param implements   sequence of strings representing interfaces
    * @param parentInterface   potential superclass
    * @return
    */
  def makeInterface(pkgName:String, className:String, implements:Seq[String] = Seq.empty, parentInterface:Option[String] = None) : GJ = {
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
    GJ(s"""|$packageClause
             |public interface $className $extendsClause $implementsClause {
             |
             |}""".stripMargin)
  }


}
