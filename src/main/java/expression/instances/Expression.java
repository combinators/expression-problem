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
 *
 * Exposes all dependent operations and types
 */
public class Expression implements Iterable<UnitTest> {

    class Collector extends Visitor {
        ArrayList<Exp> types = new ArrayList<>();

        public void visit(Instance inst) {
            Exp e = inst.self();
            if (!types.contains(e)) {
                types.add(e);
            }
        }
    }

    /** The expression itself. */
    public final Instance expression;

    /** Unit Suite. */
    final ArrayList<UnitTest> tests = new ArrayList<>();

    /**
     * Construct an expression for a given domain model.
     */
    public Expression (Instance expression) {
        this.expression = expression;
    }

    public Iterator<UnitTest> iterator() {
        return tests.iterator();
    }

    /** Returns all Exp types in all tests. */
    public Iterator<Exp> dependentTypes() {
        Collector c = new Collector();
        for (UnitTest ut : tests) {
            ut.inst.accept(c);
        }
        return c.types.iterator();
    }

    /** Return all operations testes across all tests. */
    public Iterator<Operation> operations() {
        ArrayList<Operation> ops = new ArrayList<Operation>();
        for (UnitTest ut : tests) {
            if (!ops.contains(ut.op)) {
                ops.add(ut.op);
            }
        }

        return ops.iterator();
    }

    /**
     * For this expression, ensure that the operation produces the given value.
     *
     * Must be validated by the domain model to ensure that the operation even exists.
     */
    public boolean add(Operation op, Object expected) {
        tests.add(new UnitTest(expression, op, expected));
        return true;
    }

    /**
     * Ensures that the given domain model has the necessary types and operators to support
     * the given expression.
     */
    public void validate(DomainModel model) {

        // get all instances from the expression, and make sure domain model contains them
        expression.subInstances().forEach(inst -> {
            Exp exp = inst.self();
            // make sure the inst is either a type or an operation
            if (!model.data.contains(exp)) {
                System.err.println("Expression " + expression.toString() +
                        " contains type " + inst.toString() +
                        " that doesn't appear in its associated domain model.");
            }
        }
        );
    }
}
