package expression.instances;

import expression.data.Add;
import expression.extensions.Divd;
import expression.extensions.Mult;

public class SimpleStructure extends Structure {
    @Override

    Instance build() {
        // ( ( 5 * 7 ) + ( 8 / 9 ) )
        Instance mult = new expression.instances.BinaryExp(new Mult(),
                new expression.instances.Lit(5), new expression.instances.Lit(7));

        Instance div = new expression.instances.BinaryExp(new Divd(),
                new expression.instances.Lit(8), new expression.instances.Lit(9));

        Instance add = new expression.instances.BinaryExp(new Add(),
                mult, div);

        return add;
    }
}
