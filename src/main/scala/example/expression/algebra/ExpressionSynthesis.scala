package example.expression.algebra

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.stmt.Statement
import expression._
import org.combinators.cls.types._
import org.combinators.templating.twirl.Java
import expression.data.Eval
import expression.history.History
import expression.types.Types
import shared.compilation.CodeGeneratorRegistry

import scala.collection.JavaConverters._


/** Future work to sanitize combinators to be independent of Exp. */
trait ExpressionSynthesis extends SemanticTypes with Registry {

  var codeGenerator:CodeGeneratorRegistry[Seq[Statement]]

  class BaseExpClass(dm: DomainModel, vers: Type) {
    // have a domain object


    def apply(): CompilationUnit = {

      val signatures = dm.data.asScala
        .map(sub => { // sub is either 'lit' or 'add'
          val subName = sub.getClass.getSimpleName.toLowerCase    // java etiquette methods are lower case
          // build up sequence
          var params: Seq[String] = Seq.empty
          sub.ops.asScala.foreach {
            case att: Attribute =>
              val tpe = if (att.attType == Types.Exp) {
                "E" // perhaps a hack?
              } else {
                Type_toString(att.attType)
              }

              params = params :+ s"$tpe ${att.attName}"
            case _ =>
          }

          // creates method signature from parameters
          val paramList = params.mkString(",")

          s"""E ${subName}($paramList);"""

        }).mkString("\n")

      Java(s"""package algebra;
              |
              |interface ExpAlg<E> {
              | $signatures
              |}
              |
              |""".

          stripMargin).compilationUnit()
}
    val semanticType:Type = domain_evolution(domain_evolution.baseClass, vers)
  }


  class ExtendedInterface(dm: DomainModel,newName:String, previous:String, vers:Type) {
    // have a domain object


    def apply(): CompilationUnit = {

      val signatures = dm.data.asScala
        .map(sub => {  // sub is either 'lit' or 'add' (or 'sub')
          val subName = sub.getClass.getSimpleName.toLowerCase // build up sequence -- java etiquette
          var params:Seq[String] = Seq.empty
          sub.ops.asScala.foreach {
            case att: Attribute =>
              val tpe = if (att.attType == Types.Exp) {
                "E"   // perhaps a hack?
              } else {
                Type_toString(att.attType)
              }

              params = params :+ s"$tpe ${att.attName}"
            case _ =>
          }

          // creates method signature from parameters
          val paramList = params.mkString(",")

          s"""E ${subName}($paramList);"""

        }).mkString("\n")

      Java(s"""package algebra;
              |
              |interface ${newName}ExpAlg<E> extends ${previous} {
              | $signatures
              |}
              |
              |""".stripMargin).compilationUnit()
    }
    val


    semanticType:Type = domain_evolution(domain_evolution.extendedInterface , vers)
  }



  class BaseClass(expr: Exp, parent:String) {
    def apply(): CompilationUnit = {

      // build up a sequence of parameters right from the Expr definition
      var params: Seq[String] = Seq.empty
      expr.ops.asScala.foreach {
        case att: Attribute =>
          val tpe = Type_toString(att.attType)
          params = params :+ s"$tpe ${att.attName}"
        case _ =>
      }

      // comma-separate these to be proper syntax within method
      val paramList = params.mkString(",")

      val name = expr.getClass.getSimpleName.toLowerCase
      val iName= expr.getClass.getSimpleName
      Java(s"""package algebra; interface ${iName}ExpAlg extends $parent<E>
           |{
           |        E $name($paramList);
           |}""".stripMargin).compilationUnit()
    }

    // semantic type is based on the subclass (i.e., it will be exp('Base, 'Lit) or exp('Base, 'Add)
    val
    semanticType:Type = evolved_exp(exp.base, expr, parent)
  }



  // purpose of this class is to take an operation and produce
  // class NAME_ExpAlg implements ExpAlg<NAME> {
  //     relevant methods
  // }
  // i.e., class EvalExpAlg implements ExpAlg<Eval> {
  //
  class OperationBaseClass(dm: DomainModel, op:Operation) {
    def apply(): CompilationUnit= {
      // this gets "eval" and we want the name of the Interface.
      //val name = op.name
      val name = op.getClass.getSimpleName // name.capitalize
      val returnType = Type_toString(op.`type`)     // allows direct access to java field with reserved token name



      val methods = dm.data.asScala
        .map(sub => {  // sub is either 'lit' or 'add'

          val subName = sub.getClass.getSimpleName.toLowerCase   // to get proper etiquette for method names
          // build up sequence

          //val function = registry.getImplementation(op)
         // val value:MethodDeclaration = function.get(sub.getClass).get
          //val signatures = value.getBody.get.toString()
         val code:Seq[Statement] = codeGenerator((op, sub.getClass)).get
          val signatures = code.mkString("\n")

          var params:Seq[String] = Seq.empty
          sub.ops.asScala.foreach {
            case att: Attribute =>
              val tpe = if (att.attType == Types.Exp) {
                  name
              } else {
                Type_toString(att.attType)
              }

              params = params :+ s"final $tpe ${att.attName}"
            case _ =>
          }

          // creates method body
          val paramList = params.mkString(",")
          s"""
             |public ${name} ${subName}(${paramList}) {
             |        return new ${name}() {
             |            public ${returnType} eval() {
             |                $signatures
             |            }
             |        };
             |    }
           """.stripMargin

        }
        ).mkString("\n")

      Java(s"""package algebra;
            |
            |class ${name}ExpAlg implements ExpAlg<${name}> {
            |     $methods
            |}
            |""".stripMargin).compilationUnit()
  }

    val semanticType:Type =  ops (ops.baseClass,new Eval)
  }


  def mostRecentData(hist:History): Exp = {

    hist.asScala.foreach (domain =>
      if (!domain.data.isEmpty) {
        return domain.data.get(0)
      }
    )

    return null   // shouldn't get here...
  }

  // class EvalSubExpAlg extends EvalExpAlg implements SubExpAlg<Eval> {
  // HACK: Shouldn't be forced to require only just ONE sub expression
  // HACK: NEED TO FIX THIS...
  class OperationExtendedBaseClass(hist:History, tag:String, exp:Exp, op:Operation, vers:Type) {
    def apply(): CompilationUnit = {
      // this gets "eval" and we want the name of the Interface.
      val name = op.name
      val lowername = name.toLowerCase    // java etiquette for method names
      val expName = exp.getClass.getSimpleName
      val lowerExpName = expName.toLowerCase
      val className = op.getClass.getSimpleName
      val returnType = Type_toString(op.`type`)

      // grab method declaration from (op x exp)
      //val function = registry.getImplementation(op)
      //val value:MethodDeclaration = function.get(exp.getClass).get
      //val signatures = value.getBody.get.toString()
      val code:Seq[Statement] = codeGenerator((op, exp.getClass)).get
      val signatures = code.mkString("\n")

      // build up sequence of parameters for that exp sub-variant
      var params: Seq[String] = Seq.empty
      exp.ops.asScala.foreach {
        case att: Attribute =>
          val tpe = if (att.attType == Types.Exp) {
            className     // algebraic co-variant refinement (??) expName
          } else {
            Type_toString(att.attType)
          }

          params = params :+ s"final $tpe ${att.attName}"
        case _ =>
      }

      val paramList = params.mkString(",")
      val method = s"""
                      |public $className $lowerExpName($paramList) {
                      |        return new $className() {
                      |            public $returnType $lowername() {
                      |               $signatures
                      |            }
                      |        };
                      |    }
       """.stripMargin

      val lastParent = mostRecentData(hist)
      var parent = "EvalExpAlg"   // hack??
      if (lastParent != null) {
        parent = s"${lastParent.getClass.getSimpleName}ExpAlg"
      }

     Java(s"""package algebra;
                   |
              |class $className${expName}ExpAlg extends $parent implements ${expName}ExpAlg<$className> {
                   |    $method
                   |}
                   |""".stripMargin).compilationUnit()
    }

    val semanticType:Type = domain_evolution(domain_evolution.extendedData, vers) // a bit of a hack. Start with getting this working then fix later.
  }

  // This should be merged with other class. But for now, tries to deal with special
  // cases (1) New data variant, must find most recent operation
  class OperationSpecialExtendedBaseClass(dm:DomainModel, exp:Exp, op:Operation, vers:Type) {
    def apply(): CompilationUnit = {
      // this gets "eval" and we want the name of the Interface.
      val name = op.name
      val lowername = name.toLowerCase   // java etiquette
      val expName = exp.getClass.getSimpleName
      val lowerExpName = expName.toLowerCase   // doesn't look right...
      val className = op.getClass.getSimpleName
      val returnType = Type_toString(op.`type`)


      // Here assume the MethodDeclaration actually exists.
      // HACK of sorts: Still uses visitor idea to get visitor method but then we only want code.
//      val function = registry.getImplementation(op)
//      val value:MethodDeclaration = function.get(exp.getClass).get
      //val signatures = value.getBody.get.toString()
      val code:Seq[Statement] = codeGenerator((op, exp)).get
      val signatures = code.mkString("\n")
      // build up sequence of parameters for that exp sub-variant
      var params: Seq[String] = Seq.empty
      exp.ops.asScala.foreach {
        case att: Attribute =>
          val tpe = if (att.attType == Types.Exp) {
            expName
          } else {
            Type_toString(att.attType)
          }

          params = params :+ s"final $tpe ${att.attName}"
        case _ =>
      }

      val paramList = params.mkString(",")
      val method = s"""
                      |public $className $lowerExpName($paramList) {
                      |        return new $className() {
                      |            public $returnType $lowername() {
                      |               $signatures
                      |            }
                      |        };
                      |    }
       """.stripMargin



      val str = s"""package algebra;
                   |
              |class ${className}${expName}ExpAlg extends ${className}ExpAlg implements ${expName}ExpAlg<$className> {
                   |    $method
                   |}
                   |""".stripMargin

      Java(str).compilationUnit()
    }

    val semanticType:Type = domain_evolution(domain_evolution.extendedData, vers) // a bit of a hack. Start with getting this working then fix later.
  }


  /**
    * Recursively grabs all exp subvariants from the evolution history.
    *
    * @param hist   DomainModel history
    * @return
    */
  def allVariants(hist:History): Seq[Exp] = {
    var exps:Seq[Exp] = Seq.empty

    hist.asScala.foreach (domain =>
      domain.data.asScala.foreach (exp =>
        exps = exps :+ exp
      )
    )

    return exps
  }

  // domainModel represents the 'last one' and we can pull together all prior exp variants
  class OperationSpecialImpClass(dm:DomainModel,op:Operation,previousExp:String, previous:String, vers: Type) {
    def apply(): CompilationUnit= {
      // this gets "eval" and we want the name of the Interface.
      //val name = op.name
      val name = op.getClass.getSimpleName
      val lowername = name.toLowerCase    // java etiquette for method names
      val returnType = Type_toString(op.`type`)  // allows direct access to java field with reserved token name//hacking

      val methods:String = dm.data.asScala.map(sub => {
        // sub is either 'lit' or 'add'
        val subName = sub.getClass.getSimpleName
       // val function = registry.getImplementation(op)

        // Here assume the MethodDeclaration actually exists.
        // HACK of sorts: Still uses visitor idea to get visitor method but then we only want code.
        //val value:MethodDeclaration = function.get(sub.getClass).get
       // val signatures = value.getBody.get.toString()
        val code:Seq[Statement] = codeGenerator((op, sub.getClass)).get
        val signatures = code.mkString("\n")

        // build up sequence  
        var params:Seq[String] = Seq.empty
        sub.
          ops.
          asScala.foreach {
          case att: Attribute =>
            val tpe = if (att.
              attType == Types.Exp) {
              name
            }
            else {
              Type_toString(att.attType)
            }
            params = params :+ s"final $tpe ${att.attName}"
          case _ =>
        }
        // creates method body
        val paramList = params.mkString(",")

        s"""
           |public ${name} ${subName.toLowerCase}(${paramList}) {
           |        return new ${name}() {
           |            public ${returnType} ${lowername}() {
           |               $signatures
           |            }
           |        };
           |    }
       """.stripMargin

      }).mkString("\n")


      Java(s"""package algebra;
              |
              |class ${name}${previous}ExpAlg extends ${name}${previousExp} implements ${previous}ExpAlg<${name}> {
              |    $methods
              |}
              |""".stripMargin).compilationUnit()

    }
    val semanticType: Type =  evolved2_ops(evolved_ops.base, op, previousExp, previous)
    //val semanticType: Type =  domain_evolution(domain_evolution.extendedOp, vers)
  }

//  evolved_ops {
//    def apply (phase:Type, op:Operation, exp:Exp, parent:String) : Constructor =



    // domainModel represents the 'last one' and we can pull together all prior exp variants
  class OperationImpClass(hist:History, tag:String, op:Operation,previous:String, vers: Type) {
    def apply(): CompilationUnit= {
      // this gets "eval" and we want the name of the Interface.
      //val name = op.name
      //
      // val name =op.name.capitalize
      val name = op.getClass.getSimpleName
      val lowername = name.toLowerCase
      val returnType = Type_toString(op.`type`)  // allows direct access to java field with reserved token name//hacking
      //val function = registry.getImplementation(op)

      val methods:String = allVariants(hist).map(sub => {
          // sub is either 'lit' or 'add'
        val subName = sub.getClass.getSimpleName.toLowerCase   // java etiquette

        //val value:MethodDeclaration = function.get(sub.getClass).get
        //val signatures = value.getBody.get.toString()
        val code:Seq[Statement] = codeGenerator((op, sub.getClass)).get
        val signatures = code.mkString("\n")

        var params:Seq[String] = Seq.empty
          sub.
            ops.
            asScala.foreach {
            case att: Attribute =>
              val tpe = if (att.
                attType == Types.Exp) {
                name
              }
              else {
                Type_toString(att.attType)
              }
              params = params :+ s"final $tpe ${att.attName}"
            case _ =>
          }
          // creates method body
          val paramList = params.mkString(",")

          s"""
             |public ${name} ${subName}(${paramList}) {
             |        return new ${name}() {
             |            public ${returnType} ${lowername}() {
             |              $signatures
             |            }
             |        };
             |    }
       """.stripMargin

        }).mkString("\n")

      Java(s"""package algebra;
              |
              |class ${name}ExpAlg implements ${previous}ExpAlg<${name}> {
              |    $methods
              |}
              |""".stripMargin).compilationUnit()

    }

    val semanticType: Type =  domain_evolution(domain_evolution.extendedOp, vers)
  }

    // interface SubExpAlg<E> extends ExpAlg<E> {
    //  E sub(E e1, E e2);


    //}

  // Updating evaluation:

  //class EvalSubExpAlg extends EvalExpAlg implements SubExpAlg<Eval> {
  //public Eval sub(final Eval e1, final Eval e2) {

    //return new Eval() {
        //public int eval() {
    //return e1.eval() - e2.eval();
       // }
     // };
    //}
  //}// use this one for the base interface Eval [as well as any other operation], such

    // as PPrint
  class BaseInterface(op: Operation) {
      def apply: CompilationUnit = {

        val name = op.getClass.getSimpleName.toLowerCase    // follow java etiquette
        val iName= op.getClass.getSimpleName.capitalize
        val tpe = Type_toString(op.`type`)

        //implementations
        Java(s"""|package algebra;
               |interface $iName{
               |  $tpe $name();
               |}""".stripMargin).

          compilationUnit()
    }

    val semanticType: Type = ops (ops.baseInterface,op)
  }
}





