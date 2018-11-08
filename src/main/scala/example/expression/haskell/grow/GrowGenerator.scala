package example.expression.haskell.grow     /*DI:LD:AD*/

import java.nio.file.Paths

import example.expression.domain.{BaseDomain, ModelDomain}
import example.expression.haskell._

trait GrowGenerator extends HaskellGenerator with StandardHaskellBinaryMethod with HaskellBinaryMethod {
  val domain:BaseDomain with ModelDomain
  import domain._

  def getModel: domain.Model

  lazy val flat:domain.Model = getModel.flatten()

  /** For the processed model, return generated code artifacts for solution. */
  def generatedCode():Seq[HaskellWithPath] = {
      getModel.inChronologicalOrder.map(m => generateEvolution(m)) :+
      generateDataTypes(flat)
  }


  /** Combined string from the types. */
  def extTypeDeclaration(m:Model):String = {
    extDeclaration(m) + "Type"
  }

  def onlyTypes(m:Model):String = {
    m.types.map(t => t.name.capitalize).mkString("")
  }

  /** Combined string from the types. */
  def extDeclaration(m:Model):String = {
    onlyTypes(m) + "Ext"
  }

  /** Exp defined solely by types. */
  def expDeclaration(m:Model):String = {
    if (m.last.isEmpty) {
      domain.baseTypeRep.name
    } else {
      onlyTypes(m) + domain.baseTypeRep.name
    }
  }

  /**
    * Handles refinement of SubExp f ~ ExpExtType for all predecssor types
    *
    * @param m
    * @param op
    * @return
    */
  def generateOp(m:Model, op:Operation) : Haskell = {
    val mcaps = m.name.capitalize    // haskell needs data to be capitalized!
    val baseDomain = domain.baseTypeRep.name
    val name = op.name

    val returnType = typeConverter(op.returnType.get)
    val extType = extTypeDeclaration(m)

    val inner:String= m.types.map(exp => {
      val head = exp match {
        case b: Binary => s"$name${expDeclaration(m)} helpWith "
        case u: Unary => s"$name${expDeclaration(m)} helpWith "
        case _ => s"$name${expDeclaration(m)} _ "
      }
      val rest = s"(${exp.name.capitalize} ${standardArgs(exp).getCode}) = " + logic(exp)(op).mkString("\n")

      val modifiedRest = if (!m.last.isEmpty) {
        // must embed 'help' properly, if needed
        val code = logic(exp)(op).mkString("\n")
        if (code.contains(" helpWith ")) {
          s"""(${exp.name.capitalize} ${standardArgs(exp).getCode}) =
             #  let help = $name${expDeclaration(m)} helpWith in
             #  ${code.replace(" helpWith ", " help ")}""".stripMargin('#')
        } else {
          rest
        }
      } else {
        rest
      }
      head + modifiedRest
    }).mkString("\n")

    val previous:String = if (m.last.isEmpty) {
      "Void"
    } else {
      extTypeDeclaration(m) + " " + m.name.capitalize
    }

    // capture inner extension relationships
    val header = s"($extType f -> $returnType)"
    val signature = if (!m.last.isEmpty) {
      // Must remove the lastmost "empty" one, as well as the one before it, since we don't need ~ arguments
      // for the first definition in M0
      val prior = m.toSeq.filterNot(m => m.isEmpty || m.last.isEmpty).map(m =>
         s"${expDeclaration(m)} f ~ ${extTypeDeclaration(m.last)} f")
      "(" + prior.mkString(",") + s") => $header"
    } else {
      header
    }

    new Haskell(s"""
         #-- | Evaluates  expression.
         #$name${expDeclaration(m)}
         #  :: $signature
         #  -- ^ Function to help with extensions
         #  -> ${expDeclaration(m)} f
         #  -- ^ The expression to evaluate
         #  -> $returnType
         #$inner
         #$name${expDeclaration(m)} helpWith (${extDeclaration(m)} inner) = helpWith inner
         #
         #-- | Helps with extensions $mcaps
         #helpWith${op.name.capitalize}$mcaps :: $previous -> ${typeConverter(op.returnType.get)}
         #helpWith${op.name.capitalize}$mcaps = absurd
         #
         #-- | Evaluates an $mcaps expression
         #-- | Calls ${op.name}$baseDomain with the $mcaps helper
         #${op.name}$baseDomain$mcaps :: ${expDeclaration(m)} $mcaps -> ${typeConverter(op.returnType.get)}
         #${op.name}$baseDomain$mcaps e = ${op.name}${expDeclaration(m)} helpWith${op.name.capitalize}$mcaps e
         #""".stripMargin('#'))
  }



  def generateData(m:Model):Haskell = {
    val mcaps = m.name.capitalize    // haskell needs data to be capitalized!
    val Exp = domain.baseTypeRep.name

    val inner:String= m.types.map(t =>
      t match {
        case b:Binary => s"""${t.name.capitalize} ($Exp f) ($Exp f)     -- Binary instance """
        case c:Unary =>  s"""${t.name.capitalize} ($Exp f)              -- Unary instance """
        case a:Atomic => s"""${t.name.capitalize} ${typeConverter(t.attributes.head.tpe)}    -- Atomic instance """
      }
    ).mkString("\n     | ")

    val ops:String = m.ops.map(op => generateOp(m, op)).mkString("\n")

    var pastExtensions:String = ""
    var now = m
    while (!now.last.isEmpty) {
        val past = now.last
        pastExtensions = s"type instance ${extTypeDeclaration(past)} $mcaps = ${onlyTypes(m)}Exp $mcaps\n" + pastExtensions
        now = now.last
    }

    // must find PAST operations and incorporate them here
    val pastOps = m.last.pastOperations().map(op => generateOp(m, op)).mkString("\n")

    new Haskell(s"""
            #-- | Datatype for arithmetic.
            #-- | Parameter f is to be filled with the marker type of the
            #-- | current evolution.
            #data ${expDeclaration(m)} f = $inner
            #     | ${extDeclaration(m)} (${extTypeDeclaration(m)} f)    -- Datatype extensions
            #
            #-- | Family of Exp data-type extensions:
            #-- | Given a marker type of a evolution, compute the type extension
            #-- | of Exp used for this evolution.
            #type family ${extTypeDeclaration(m)} f
            #
            #$ops
            #
            #-- Evolution $mcaps
            #-- | Marker type to select evolution $mcaps
            #data $mcaps
            #
            #-- | Selecting $mcaps means: no extensions to type ${expDeclaration(m)}; take care of previous ones
            #$pastExtensions
            #type instance ${extTypeDeclaration(m)} $mcaps = Void
            #
            #$pastOps
            #""".stripMargin('#'))   // HACK: Issue with "|"
  }

  /**
    * Each evolution has chance to add data extensions and functional extensions
    *
    * @param m
    * @return
    */
  def generateEvolution(m:Model) : HaskellWithPath = {

    var pastImports:String = ""

    var past = m.last
    while (!past.isEmpty) {
      pastImports = s"import ${past.name.capitalize}\n" + pastImports
        past = past.last
    }

    // HACK: Awkward to use stripmargin, since generateData might start with "|" char in Haskell!!
    val code = Haskell(s"""|{-# LANGUAGE TypeFamilies #-}
                           |module ${m.name.capitalize} where
                           |
                           |import GHC.Types (Constraint)
                           |import Data.Void
                           |$pastImports
                           |""".stripMargin + generateData(m))

    HaskellWithPath(code, Paths.get(s"${m.name.capitalize}.hs"))
  }

    /**
      * Responsible for dispatching sub-expressions with possible parameter(s).
      * Seems safest to include/embed parens here.
      *
      * middle operator is 'helpWith' or 'help' for extensions, and this needs to be 'fixed' by
      * the one calling logic. This is not an ideal solution but it works
      */
    override def dispatch(primary:Haskell, op:domain.Operation, params:Haskell*) : Haskell = {
      val args:String = params.mkString("")

      Haskell(s"""(${op.name}${domain.baseTypeRep.name} helpWith ${primary.toString} $args)""")
    }

  /**
    * Determines the Haskell expression for all children of a Exp subtype based on its attributes.
    *
    * For example, an expressions.BinaryExp has 'left' and 'right' attributes, whereas an
    * expressions.UnaryExp only has an 'exp'
    */
  def subExpressions(exp:domain.Atomic) : Map[String, Haskell] = {
    exp.attributes.map(att => att.name -> Haskell(s"${att.name}")).toMap
  }
}