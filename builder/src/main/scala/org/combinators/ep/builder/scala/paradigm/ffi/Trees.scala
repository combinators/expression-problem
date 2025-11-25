import org.combinators.cogen.TypeRep
import org.combinators.cogen.TypeRep.OfHostType
import org.combinators.ep.domain.abstractions.DomainTpeRep
import org.combinators.ep.language.inbetween.{any, oo}
import org.combinators.ep.language.scala
import org.combinators.ep.language.scala.Finalized.{FinalTypes, Type} /*CreateLeaf, */
import org.combinators.ep.language.scala.{Expression, Factory, FinalTypes, Type}
/*
trait CreateLeaf[FT <: FinalTypes] extends TreeOps.CreateLeaf[FT] with Type[FT] {
  def leafClass: oo.ClassReferenceType[FT]
  def toScala: String = leafClass.toScala

  def copyWithLeafClass(leafClass: oo.ClassReferenceType[FT] = leafClass): TreeOps.CreateLeaf[FT] =
    createLeafWithLeafClass(leafClass)

  def prefixRootPackage(rootPackageName: Seq[any.Name[FT]], excludedTypeNames: Set[Seq[any.Name[FT]]]): TreeOps.CreateLeaf[FT] =
    copyWithLeafClass(leafClass = leafClass.prefixRootPackage(rootPackageName, excludedTypeNames))

  def toImport: Seq[any.Import[FT]] = leafClass.toImport
}

trait CreateNodeExpr[FT <: FinalTypes] extends TreeOps.CreateNodeExpr[FT] with Expression[FT] {
  def nodeClass: oo.ClassReferenceType[FT]
  def toScala: String = nodeClass.toScala

  def copyWithNodeClass(nodeClass: oo.ClassReferenceType[FT] = nodeClass): TreeOps.CreateNodeExpr[FT] =
    createNodeExprWithNodeClass(nodeClass)

  def prefixRootPackage(rootPackageName: Seq[any.Name[FT]], excludedTypeNames: Set[Seq[any.Name[FT]]]): TreeOps.CreateNodeExpr[FT] =
    copyWithNodeClass(nodeClass = nodeClass.prefixRootPackage(rootPackageName, excludedTypeNames))
}

trait Factory {
  override def createNodeExpr(): TreeOps.CreateNodeExpr[FT] = {
    createNodeExprWithNodeClass(classReferenceType(
      Seq("org", "combinators", "ep", "util", "Node").map(n => name(n, n)) *
    ))
  }
  def createNodeExprWithNodeClass(nodeClass: oo.ClassReferenceType[FT]): CreateNodeExpr[FT]

  override def createLeaf(): TreeOps.CreateLeaf[FT] = {
    createLeafWithLeafClass(classReferenceType(
      Seq("org", "combinators", "ep", "util", "Leaf").map(n => name(n, n)) *
    ))
  }
  def createLeafWithLeafClass(leafClass: oo.ClassReferenceType[FT]): CreateLeaf[FT]


  implicit def convert(other: TreeOps.CreateLeaf[FT]): CreateLeaf[FT]
  implicit def convert(other: TreeOps.CreateNodeExpr[FT]): CreateNodeExpr[FT]

  def createNodeExprWithNodeClass(nodeClass: oo.ClassReferenceType[FinalTypes]): scala.CreateNodeExpr[FinalTypes] = CreateNodeExpr(nodeClass)
  def createLeafWithLeafClass(leafClass: oo.ClassReferenceType[FinalTypes]): scala.CreateLeaf[FinalTypes] = CreateLeaf(leafClass)
  implicit def convert(other: TreeOps.CreateLeaf[FinalTypes]): scala.CreateLeaf[FinalTypes] = other.getSelfCreateLeaf
  implicit def convert(other: TreeOps.CreateNodeExpr[FinalTypes]): scala.CreateNodeExpr[FinalTypes] = other.getSelfCreateNodeExpr


  override type CreateLeaf = Finalized.CreateLeaf
  override type CreateNodeExpr = Finalized.CreateNodeExpr

  case class CreateLeaf(override val leafClass: oo.ClassReferenceType[FinalTypes]) extends scala.CreateLeaf[FinalTypes] with Type {
    def getSelfCreateLeaf: this.type = this
  }

  case class CreateNodeExpr(override val nodeClass: oo.ClassReferenceType[FinalTypes]) extends scala.CreateNodeExpr[FinalTypes] with Expression {
    def getSelfCreateNodeExpr: this.type = this
  }

}

trait ReifiedScalaValue[FT <: FinalTypes, T] extends Expression[FT] with Factory[FT] {
  def getSelfAsReifiedScalaValue: finalTypes.ReifiedScalaValue[T]
  val ofHostType: OfHostType[T]
  val value: T

  def toScala: String = {
    ofHostType match {
      case t: TypeRep.String.type => s""""$value""""
      case t: DomainTpeRep.Tree.type =>
        value match {
          case org.combinators.ep.domain.tree.Node(id, values) => s"org.combinators.ep.util.Node($id, ${values.map(v => reifiedScalaValue(DomainTpeRep.Tree, v).toScala).mkString(", ")})"
          case org.combinators.ep.domain.tree.Leaf(r) => s"org.combinators.ep.util.Leaf(${reifiedScalaValue(r.tpe, r.inst).toScala})"
        }
      case t: TypeRep.Sequence[_] =>
        value.asInstanceOf[Seq[t.elemTpe.HostType]].map(v => reifiedScalaValue(t.elemTpe, v).toScala).mkString("Seq(", ", ", ")")
      case t: TypeRep.Array[_] =>
        value.asInstanceOf[Array[t.elemTpe.HostType]].map(v => reifiedScalaValue(t.elemTpe, v).toScala).mkString("Array(", ", ", ")")
      case _ =>
        value.toString
    }
  }

  override def prefixRootPackage(rootPackageName: Seq[any.Name[FT]], excludedTypeNames: Set[Seq[any.Name[FT]]]): ReifiedScalaValue[FT, T] =
    this
}*/