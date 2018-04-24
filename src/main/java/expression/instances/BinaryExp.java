package expression.instances;

/**
 * Represents an actual Lit instance.
 */
public class BinaryExp implements Instance {
    /** The actual operator. */
    final expression.data.BinaryExp op;
    final Instance left;
    final Instance right;

    public BinaryExp(expression.data.BinaryExp op, Instance left, Instance right) {
        this.op = op;
        this.left = left;
        this.right = right;
    }
}
