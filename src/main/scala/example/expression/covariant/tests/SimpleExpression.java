package example.expression.covariant.tests;

import expression.DomainModel;
import expression.data.Add;
import expression.data.Lit;
import expression.extensions.Divd;
import expression.extensions.Mult;
import expression.instances.Expression;
import expression.instances.Instance;

public class SimpleExpression extends Expression {

    /**
     * Build expression (store it and return it as well).
     *
     * The given expression is constructed, and ultimately validated by the Domain Model
     * associated with the DomainModel.
     */
    public SimpleExpression(DomainModel model) {
        super(model);
    }

    /** This is invoked from the superclass, not directly here. */
    protected Instance expression() {
        // ( ( 5 * 7 ) + ( 8 / 9 ) )
        Instance mult = new expression.instances.BinaryExp(new Mult(),
                new expression.instances.Lit(new Lit(),5),
                new expression.instances.Lit(new Lit(),7));

        Instance div = new expression.instances.BinaryExp(new Divd(),
                new expression.instances.Lit(new Lit(),18),
                new expression.instances.Lit(new Lit(),9));

        return new expression.instances.BinaryExp(new Add(),
                mult, div);
    }


}
