package example.expression.domain

trait MC1 extends Evolution {
  self: M3 with I2 =>
  val domain:MathDomain

  // m3 x i2:model evolution.
  // -------------------
  override def getModel = m3.merge("c1", i2)
}
