package expression.instances;

import expression.Operation;

/**
 * Represents a test case.
 *
 * For the given element, e, and operation, op, the result is the given Object.
 *
 * Querying the result is up to the actual code generator. Here we are trying to record
 * sufficient information that can be used to generate a real JUnit test case
 */
public class UnitTest {
    public final Operation op;
    public final Instance inst;
    public final Object expected;

    public UnitTest(Instance inst, Operation op, Object expected) {
        this.inst = inst;
        this.op = op;
        this.expected = expected;
    }
}
