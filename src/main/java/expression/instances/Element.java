package expression.instances;

import expression.Exp;

import java.util.HashMap;

/**
 * Represents an element in an expression.
 *
 * ( (5 * 7 ) + ( 8 * 9 ) )
 *
 * MULT is an element, as are each of the numbers
 * sub-expression (5*7) is an element
 *
 */
public class Element {
    final Class<? extends Exp> base;
    final HashMap<String, Object> associated = new HashMap<>();

    public Element (Class<? extends Exp> clazz) {
        base = clazz;
    }

    public void put (String key, Object val) {
        associated.put(key, val);
    }

    public Object get (String key) {
        return associated.get(key);
    }

}
