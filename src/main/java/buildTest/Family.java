package buildTest;

import java.util.ArrayList;
import java.util.Iterator;

public class Family implements Iterable<String> {

    public static final int MAX = 99; // max number of variations for auto generation

    final String prefix;
    String[] variations = new String[MAX];

    public Family (String pre) {
        this.prefix = pre;
    }

    public void add (int id) {
        variations[id] = prefix + id;
    }

    /** Return iterator (in reverse order) of successful variations. */
    public Iterator<String> iterator() {
        ArrayList<String> ret = new ArrayList<>();
        for (int i = MAX-1; i >= 0; i--) {
            if (variations[i] != null) {
                ret.add(prefix + i);
            }
        }

        return ret.iterator();
    }
}
