package expression.instances;

import expression.DomainModel;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Iterator;

/**
 * A Test suite is a collection of UnitTests
 */
public class UnitSuite implements Iterable<Expression> {
    protected final DomainModel model;

    ArrayList<Expression> expressions  = new ArrayList<>();

    public UnitSuite(DomainModel model) {
        this.model = model;

        // find all test cases (public methods starting with 'test' and invoke them, one after each other
        // to retrieve the expressions which are then added to our set of expressions.
        for (Method method : this.getClass().getMethods()) {
            if (method.getName().startsWith("test") &&
                method.getReturnType().equals(Expression.class)) {

                try {
                    // since static method, object is null. Model is passed as the argument
                    Expression exp = (Expression) method.invoke(null, model);
                    expressions.add(exp);
                } catch (IllegalAccessException e) {
                    e.printStackTrace();
                } catch (InvocationTargetException e) {
                    e.printStackTrace();
                }
            }
        }

    }

    /** Return each expression (and associated test cases) for this suite. */
    @Override
    public Iterator<Expression> iterator() {
        return expressions.iterator();
    }
}
