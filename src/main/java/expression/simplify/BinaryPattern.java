package expression.simplify;

import expression.Operation;

/**
 * Binary patterns are simplified either from the left, or the right, or both.
 *
 * ADD(LEFT,RIGHT) --> LEFT if (RIGHT == 0)
 * ADD(LEFT,RIGHT) --> Lit(0) if (LEFT == -RIGHT)
 * ADD(LEFT,RIGHT) --> RIGHT if (LEFT == 0)
 *
 * More complicated ones can be envisioned
 *
 * MULT(x,DIVIDE(y,x)) --> y since the x's cancel
 *
 * Note these kind of simplifications rely on multiple operations, and thus become
 * hard to describe
 *
 * In all cases, there is a guard (IF) and a result (either a fixed LIT or a new EXPR)
 *
 *
 */
public class BinaryPattern extends Pattern {

    public enum Side {
        LEFT("left"), RIGHT("right"), NEGLEFT("-left"), NEGRIGHT("-right");

        public final String name;

        Side(String s) {
            this.name = s;
        }
    }

    public BinaryPattern(Operation op) {

    }
}

