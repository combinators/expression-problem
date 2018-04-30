package expression.instances;

import expression.Exp;

import java.util.stream.Stream;

/**
 * Represents an actual Lit instance.
 */
public class Lit implements Instance {
    public final double value;
    public final Exp type;

    /**
     * Construct actual Lit, for the given literal
     * @param val
     */
    public Lit(expression.data.Lit lit, double val) {
        this.type = lit;
        this.value = val;
    }

    @Override
    public String toString() {
        return "" + value;
    }

    @Override
    public Stream<Instance> subInstances() {
        return Stream.of(this);
    }

    @Override
    public Exp self() {
        return type;
    }

    /** Standard visitor pattern. Uses post-order visit traversal. */
    public void accept(Visitor v) {
       v.visit(this);
    }
}
