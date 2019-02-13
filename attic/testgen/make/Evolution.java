package make;

import java.util.*;

public class Evolution {

    static Hashtable<String, Evolution> map = new Hashtable<>();

    final String prior;
    final String name;

    public Evolution (String name) {
        this.prior = null;
        this.name = name;
        map.put(name, this);
    }

    public Evolution (String name, String prior) {
        this.name = name;
        this.prior = prior;
        map.put(name, this);
    }

    boolean isSimple() { return true; }

    public Iterator<String> evolutions() { return selfAndPast(name); }

    // prior evolutions (assumes no cycles!)
    public Iterator<String> selfAndPast(String n) {
        ArrayList<String> names = new ArrayList<>();
        while (n != null) {
            names.add(n);
            if (map.containsKey(n)) {
                Evolution e = map.get(n);
                n = e.prior;
            } else {
                System.out.println ("ERROR: no prior evolution" + n);
                break;
            }
        }

        return names.iterator();
    }
}
