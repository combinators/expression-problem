package expression.extensions;

import expression.Operation;
import expression.types.Types;

/**
 * Given an expression, simplify it (i.e., "5+0" --> "5", or "8*1" --> "8" or "8*0" --> "0").
 *
 * Different kinds of simplifications become possible over time, each of which would extend this
 */
public abstract class SimplifyBase extends Operation {

    public SimplifyBase() {
        super("simplify", Types.Exp);
    }
}