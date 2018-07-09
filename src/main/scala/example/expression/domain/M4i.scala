package example.expression.domain

trait M4i extends Evolution {
  self: M0 with M1 with M2 with M3 with M4 =>
  val domain:MathDomain

  // mx:model evolution.
  // -------------------
  // mx: graft in a useful operation for future operations to use
  case object JavaClass extends domain.TypeRep
  case object GetJavaClass extends domain.Operation ("getJavaClass", Some(JavaClass))

  val m4i = domain.Model("mx", Seq.empty, Seq(GetJavaClass), last = m4)
  override def getModel = m4i
}
