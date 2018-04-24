package expression.instances;

/**
 * Represents an actual Lit instance.
 */
public class Lit implements Instance {
    final double val;

    public Lit(double val) {
        this.val = val;
    }
}
