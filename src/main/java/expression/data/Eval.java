package expression.data;

import expression.Operation;
import expression.types.Types;

/**
 * Todo: Explore concept of adding +/- as a unary or binary operator. In this case, eval would not
 * Todo: just have a single value, but would have multiple (not just 2, consider ((1 +/- 2) +/- 3)
 * Todo: which has four possible values (6, 0, 2, -4). This would truly be an example of a new data
 * Todo: Type which caused modifications to existing operations. note eval could just take the
 * Todo: dominant op (i.e., + in +/-, and - in -/+) which would at least be consistent.
 *
 */
public class Eval extends Operation {

    public Eval() {
        super("eval", Types.Double);
    }
}
