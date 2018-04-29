package expression.instances;

import expression.DomainModel;
import expression.Exp;
import expression.Operation;

import java.util.ArrayList;
import java.util.Iterator;

/**
 * An expression can only make sense when associated with a given domain model.
 *
 * The expression can use all constructs (types and operations) from that domain model,
 * or any of its predecessors.
 */
public abstract class Expression implements Iterable<UnitTest> {
    /** Domain model for the given expression. */
    final DomainModel model;

    /** The expression itself. */
    public final Instance expression;

    /** Unit Suite. */
    final ArrayList<UnitTest> tests = new ArrayList<>();

    /**
     * Construct an expression for a given domain model.
     * @param model
     */
    public Expression (DomainModel model) {
        this.model = model;
        expression = expression();
        validate();
    }

    public Iterator<UnitTest> iterator() {
        return tests.iterator();
    }

    /** Method responsible for constructing the actual expression. */
    protected abstract Instance expression();

    /**
     * For this expression, ensure that the operation produces the given value.
     *
     * Must be validated by the domain model to ensure that the operation even exists.
     */
    public void add(Operation op, Object expected) {
        DomainModel flat = model.flatten();

        // if domain model doesn't contain this operation, then we can't assert
        if (!flat.ops.contains(op)) {
            throw new RuntimeException ("Expression " + expression.toString() +
                    " contains operation " + op.toString() +
                    " that doesn't appear in its associated domain model.");
        }

        tests.add(new UnitTest(expression, op, expected));
    }

    /**
     * Ensures that the given domain model has the necessary types and operators to support
     * the given expression.
     */
    void validate() {
        DomainModel flat = model.flatten();

        // get all instances from the expression, and make sure domain model contains them
        expression.subInstances().forEach(inst -> {
                    Exp exp = inst.self();
                    // make sure the inst is either a type or an operation
                    if (!flat.data.contains(exp)) {
                        throw new RuntimeException ("Expression " + expression.toString() +
                                " contains type " + inst.toString() +
                                " that doesn't appear in its associated domain model.");
                    }
                }
        );
    }
}
