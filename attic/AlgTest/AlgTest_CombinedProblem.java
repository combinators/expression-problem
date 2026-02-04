class AlgTest {
	interface Algebra<R> {
	  public R lit(double value);
	  public R add(R left, R right);
	  public R sub(R left, R right);
	}

	interface AlgebraExt<R> extends Algebra<R> {
     	  public R mult(R left, R right);
	}

	interface Combined extends MultBy, Eval {}
        interface CombinedExt extends Combined, PrettyP {}


	interface MultBy {
	  public Combined multBy(Combined other);
	}
	interface MultByExt {
	  public CombinedExt multBy(CombinedExt other);
	}

	interface MultByAlg extends Algebra<MultBy> {
	  public default MultBy lit(double value) {
	    return (AlgebraProducer other) -> {
	      return (Algebra<R> alg) -> {
		Double counter = Math.floor(Math.abs(value));
		R result = other.get(alg);
		while (1.0 < counter) {
		    result = alg.add(result, other.get(alg));
		    counter = counter - 1.0;
		}
		if (this.getValue() < 0.0) {
		    result = alg.sub(alg.lit(0.0), result);
		}
		return result;
	      };
	    };
	  }
	  public default MultBy add(MultBy left, MultBy right) {
	     return (AlgebraProducer other) -> {
		return (Algebra<R> alg) -> {
			return alg.add(left.multBy(other).get(alg), right.multBy(other).get(alg));
		};
	     };
	  }
	  public default MultBy sub(MultBy left, MultBy right) {
	     return (AlgebraProducer other) -> {
		return (Algebra<R> alg) -> {
			return alg.sub(left.multBy(other).get(alg), right.multBy(other).get(alg));
		};
	     };
	  }
	}

	interface Eval {
	  public Double eval();
	}

	interface EvalAlg extends Algebra<Eval> {
	  public default Eval lit(double value) {
	    return () -> { return value; };
	  }
	  public default Eval add(Eval left, Eval right) {
	    return () -> { return left.eval() + right.eval(); };
	  }
	  public default  Eval sub(Eval left, Eval right) {
	    return () -> { return left.eval() - right.eval(); };
	  }
	}

	interface AlgebraProducerAlg extends Algebra<AlgebraProducer> {
	  public default AlgebraProducer lit(double value) {
		return (Algebra<R> alg) -> { return alg.lit(value); };
          }
          public default AlgebraProducer add(AlgebraProducer left, AlgebraProducer right) {
		return (Algebra<R> alg) -> { return alg.add(left.get(alg), right.get(alg)); };
          }
	  public default AlgebraProducer sub(AlgebraProducer left, AlgebraProducer right) {
		return (Algebra<R> alg) -> { return alg.sub(left.get(alg), right.get(alg)); };
          }
	}

	public static void main(String[] args) {
		MultByAlg multBy = new MultByAlg();
		EvalAlg evalAlg = new EvalAlg();
                AlgebraProducerAlg prodAlg = new AlgebraProducerAlg();
		System.out.println(multBy.lit(5).multBy(prodAlg.add(prodAlg.lit(1), prodAlg.lit(2))));
	}
}
