package mathdomain

class M0Test() extends org.scalatest.funsuite.AnyFunSuite {

  this.test("Test")(
    {
      assert (new mathdomain.m0.Add(new mathdomain.m0.Lit(1.0),new mathdomain.m0.Lit(2.0)).accept[Double](this.makeEval) == 3.0);
      assert (new mathdomain.m0.Lit(5.0).accept[Double](this.makeEval) == 5.0);
    }
  );

  // this was this.test("makeEval") and that is wrong
  def makeEval()
    {
    return new mathdomain.m0.Eval()
    }

}
