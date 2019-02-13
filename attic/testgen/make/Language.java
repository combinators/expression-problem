package make;

import java.util.*;

public class Language implements Iterable<String> {
    public final String name;
    final ArrayList<String> impls = new ArrayList<>();

    final ArrayList<Evolution> evolutions = new ArrayList<>();

    final Hashtable<String, String> constructors = new Hashtable<>();

    /** Mapping of evolution names to instances. */
    Instance mapping;

    public Language (String lang) { this.name = lang; }

    public Language add(String name, String scalaConstructor) {
        if (!impls.contains(name)) {
            impls.add(name);
            constructors.put(name, scalaConstructor);
        }
        return this;
    }

    public Language addMapping(Instance inst) {
        this.mapping = inst;

        return this;
    }

    /** Add a number of potential evolutions. */
    public Language addEvolutions(Evolution[] evs) {
        for (Evolution s : evs) {
            evolutions.add(s);
        }
        return this;
    }

    /** Return all evolutions. */
    public Iterator<Evolution> evolutions() {
        return evolutions.iterator();
    }

    /** Return all possible families. */
    @Override
    public Iterator<String> iterator() {
        return impls.iterator();
    }
}
