package expression.data;

import expression.Attribute;
import expression.Exp;
import expression.types.Types;

public class Lit extends Exp {
    public Lit() {
        super();

        // these methods become available. Find implementation in synthesized code
        ops.add(new Attribute("value", Types.Int));
    }

}
