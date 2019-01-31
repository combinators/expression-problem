package ep.haskell.grow     /*DI:LD:AD*/

import java.nio.file.Paths

import ep.domain.{BaseDomain, ModelDomain}
import ep.haskell._

trait GrowGenerator extends HaskellGenerator with StandardHaskellBinaryMethod with HaskellBinaryMethod {
  val domain:BaseDomain with ModelDomain
  import domain._

  def getModel: domain.Model

  lazy val flat:domain.Model = getModel.flatten()

  /** For the processed model, return generated code artifacts for solution. */
  def generatedCode():Seq[HaskellWithPath] = {
    helperClasses() ++
      getModel.inChronologicalOrder.map(m => generateEvolution(m))
  }

  /** Return designated HaskellType. */
  override def typeConverter(tpe:domain.TypeRep) : HaskellType = {
    tpe match {
      case domain.baseTypeRep => new HaskellType(s"${expDeclaration(getModel.base())} f")
      case _ => super.typeConverter(tpe)
    }
  }

  /**
    * For producer operations, there is a need to instantiate objects, and one would use this
    * method (with specific parameters) to carry this out.
    */
  override def inst(exp:domain.Atomic, params:Haskell*): Haskell = {

    val wrap = genWrap(findModel(exp))
    exp match {
      case ui: Unary =>
        Haskell(wrap(s"${ui.concept} (${params(0)}) "))

      case bi: Binary =>
        Haskell(wrap(s"${bi.concept} (${params(0)}) (${params(1)}) "))

      case exp: Atomic =>
        Haskell(wrap(s"${exp.concept} ${params(0)} "))

      case _ => Haskell(s" -- unknown ${exp.concept} ")
    }
  }

  /**
    * Extended to handle producer operations specially.
    *
    * @param m
    * @param op
    * @return
    */
  def typeSignature(m:Model, op:Operation) : String = {
    op match {
      case _:ProducerOperation =>
        val mcaps = m.name.capitalize    // haskell needs data to be capitalized!
      val baseDomain = domain.baseTypeRep.name

        s"${op.name}$baseDomain$mcaps :: ${expDeclaration(m.base())} $mcaps -> ${expDeclaration(m.base())} $mcaps"

      case _ =>
        val mcaps = m.name.capitalize    // haskell needs data to be capitalized!
      val baseDomain = domain.baseTypeRep.name

        s"${op.name}$baseDomain$mcaps :: ${expDeclaration(m.base())} $mcaps -> ${typeConverter(op.returnType.get)}"
    }
  }

  /** Combined string from the types. */
  def extTypeDeclaration(m:Model):String = {
    extDeclaration(m) + "Type"
  }

  def onlyTypes(m:Model):String = {
    m.types.map(t => t.concept).mkString("")
  }

  /** Combined string from the types. */
  def extDeclaration(m:Model):String = {
    "Ext_" + m.name.capitalize
  }

  /** Exp defined solely by types. */
  def expDeclaration(m:Model):String = {
    domain.baseTypeRep.name + "_" + m.name.capitalize
  }

  //  def typeSignature(m:Model, op:Operation):String = {
  //    val mcaps = m.name.capitalize    // haskell needs data to be capitalized!
  //    val baseDomain = domain.baseTypeRep.name
  //
  //    s"${op.name}$baseDomain$mcaps :: ${expDeclaration(m.base())} $mcaps -> ${typeConverter(op.returnType.get)}"
  //  }

  def operationForFixedLevel(m:Model, op:Operation) : String = {
    val mcaps = m.name.capitalize    // haskell needs data to be capitalized!
    val baseDomain = domain.baseTypeRep.name

    val invoke = m.inChronologicalOrder.reverse.tail.foldLeft(s"(${op.name}${expDeclaration(m)} helpWith${op.concept}$mcaps)")((former,tail) =>
      s"(${op.name}${expDeclaration(tail)} $former)")

    s"""
       #${typeSignature(m,op)}
       #${op.name}$baseDomain$mcaps e = $invoke e
       #""".stripMargin('#')
  }

  /**
    * Generates wrapper of instantiations using telescoping constructors
    *
    * @param model
    * @return
    */
  def genWrap(model: Model) : String => String = {
    if (model.base() == model) {
      (s:String) => s
    } else {
      (s:String) => {
        model.last.inChronologicalOrder.reverse.tail.foldLeft(s"${extDeclaration(model.last)} ($s)")((former,tail) =>
          s"(${extDeclaration(tail)} ($former))")
      }
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

      val modifiedRest = { // if (!m.last.isEmpty)
        // must embed 'help' properly, if needed
        val code = logic(exp, op).mkString("\n")
        if (code.contains(" helpWith ")) {
          val prior = m.last.name.capitalize

          // old:   let help = $name${expDeclaration(m.last)} ($name${expDeclaration(m)} helpWith) in


          if (!m.last.isEmpty) {
            val invoke = m.inChronologicalOrder.reverse.tail.foldLeft(s"(${op.instance}${expDeclaration(m)} helpWith)")((former,tail) =>
              s"(${op.instance}${expDeclaration(tail)} $former)")

            s"""(${exp.concept} ${standardArgs(exp).getCode}) =
               #  let help = $invoke in
               #  ${code.replace(s"$name${domain.baseTypeRep.name} helpWith ", "help ")}""".stripMargin('#')
          } else {
            s"""(${exp.concept} ${standardArgs(exp).getCode}) =
               #  let help = $name${expDeclaration(m)} helpWith in
               #  ${code.replace(s"$name${domain.baseTypeRep.name} helpWith ", "help ")}""".stripMargin('#')
          }
        } else {
          s"(${exp.concept} ${standardArgs(exp).getCode}) = " + logic(exp, op).mkString("\n")
        }
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

    // if we define new operations, we must expand as provided
    val operationSpec = operationForFixedLevel(m, op)

    new Haskell(s"""
                   #-- | Evaluates expression.
                   #$name${expDeclaration(m)}
                   #  :: $signature
                   #  -- ^ Function to help with extensions
                   #  -> ${expDeclaration(m)} f
                   #  -- ^ The expression to evaluate
                   #  -> $returnType
                   #
         #$inner
                   #$name${expDeclaration(m)} helpWith (${extDeclaration(m)} inner) = helpWith inner
                   #
         #-- | Evaluates an $mcaps expression
                   #-- | Calls ${op.instance}$baseDomain with the $mcaps helper
                   #$operationSpec
                   #
         #-- | Helps with extensions $mcaps
                   #helpWith${op.concept}$mcaps :: Void -> ${typeConverter(op.returnType.get)}
                   #helpWith${op.concept}$mcaps = absurd
                   #
         #""".stripMargin('#'))
  }   // Void had been $previous

  def generateData(m:Model):Haskell = {
    val mcaps = m.name.capitalize    // haskell needs data to be capitalized!
    val Exp = expDeclaration(m.base())    //   domain.baseTypeRep.name + "_" + m.name.capitalize

    val inner:String= m.types.map(t =>
      t match {
        case b:Binary => s"""${t.concept} ($Exp f) ($Exp f)     -- Binary instance """
        case c:Unary =>  s"""${t.concept} ($Exp f)              -- Unary instance """
        case a:Atomic => s"""${t.concept} ${typeConverter(t.attributes.head.tpe)}    -- Atomic instance """
      }
    ).mkString("\n     | ")

    val priorOps:String = if (m.ops.nonEmpty) {
      m.inChronologicalOrder.reverse.tail.reverse.flatMap(priorM => {
        m.ops.map(op => s"-- ${priorM.name.capitalize} part for ${op.instance}\n" + generateOp(priorM, op) + s"-- DONE ${priorM.name.capitalize} part\n")}).mkString("\n")
    } else { "" }

    val ops:String = m.ops.map(op => generateOp(m, op)).mkString("\n")

    var pastExtensions:String = ""
    var now = m
    while (!now.last.isEmpty) {
      val past = now.last
      pastExtensions = s"type instance ${extTypeDeclaration(past)} $mcaps = ${expDeclaration(now)} $mcaps\n" + pastExtensions
      now = now.last
    }

    // must find PAST operations and incorporate them here
    val pastOps = m.last.pastOperations().map(op => generateOp(m, op)).mkString("\n")

    val dataTypeDefinition = if (m.types.isEmpty) {
      s"newtype ${expDeclaration(m)} f = ${extDeclaration(m)} (${extTypeDeclaration(m)} f)"
    } else {
      s"""
         #data ${expDeclaration(m)} f = $inner
         #  | ${extDeclaration(m)} (${extTypeDeclaration(m)} f)    -- Datatype extensions""".stripMargin('#')
    }

    new Haskell(s"""
                   #-- | Datatype
                   #-- | Parameter f is to be filled with the marker type of the
                   #-- | current evolution.
                   #$dataTypeDefinition
                   #
            #-- | Family of Exp data-type extensions:
                   #-- | Given a marker type of a evolution, compute the type extension
                   #-- | of Exp used for this evolution.
                   #type family ${extTypeDeclaration(m)} f
                   #
            #$priorOps
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

  /** For straight design solution, directly access attributes by name. */
  override def expression (exp:Atomic, att:Attribute) : Expression = {
    Haskell(s"${att.instance}")
  }
}
