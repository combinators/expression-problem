package expression.instances;

import expression.Exp;

import java.util.stream.Stream;

/**
 * Represents an actual Binary Expression instance.
 */
public class BinaryExp implements Instance {
    /** The actual operator. */
    public final expression.data.BinaryExp op;
    public final Instance left;
    public final Instance right;

    public BinaryExp(expression.data.BinaryExp op, Instance left, Instance right) {
        this.op = op;
        this.left = left;
        this.right = right;
    }

    @Override
    public String toString() {
        return "(" + left.toString() + " " + op.getClass().getSimpleName() + " " + right.toString() + ")";
    }

    @Override
    public Stream<Instance> subInstances() {
        return Stream.concat(
                Stream.concat(
                        Stream.of(this),
                        left.subInstances()),
                        right.subInstances());
    }

    @Override
    public Exp self() {
        return op;
    }

    /** Standard visitor pattern. Uses post-order visit traversal. */
    public void accept(Visitor v) {
        v.visit(left);
        v.visit(right);
        v.visit(this);
    }
}
