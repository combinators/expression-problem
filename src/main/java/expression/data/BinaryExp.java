package expression.data;

import expression.Attribute;
import expression.Exp;
import expression.types.Types;

public abstract class BinaryExp extends Exp {
    public BinaryExp() {
        super();

        // these methods become available
        ops.add(new Attribute("left", Types.Exp));
        ops.add(new Attribute("right", Types.Exp));
    }

}
