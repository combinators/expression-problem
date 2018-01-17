package expression.data;

import expression.Attribute;
import expression.Exp;
import expression.types.Types;

public abstract class UnaryExp extends Exp {
    public UnaryExp() {
        super();

        // these methods become available
        ops.add(new Attribute("exp", Types.Exp));
    }

}
