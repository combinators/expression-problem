package make;

import java.util.*;

/**
 * Helper class to keep track of the different language implementations, and contain the
 * information needed to generate the necessary Scala code for build/deployment
 */
public class Language implements Iterable<String> {
    public final String name;
    final ArrayList<String> impls = new ArrayList<>();

    final ArrayList<Evolution> evolutions = new ArrayList<>();
    final Hashtable<String,String> domains = new Hashtable<>();
    final Hashtable<String,String> foundations = new Hashtable<>();

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
    public Language addEvolutions(String domainName, String foundationName, Evolution[] evs) {
        for (Evolution ev : evs) {
            evolutions.add(ev);
            domains.put(ev.name, domainName);
            foundations.put(ev.name, foundationName);
        }
        return this;
    }

    /** Return all evolutions. */
    public Iterator<Evolution> evolutions() {
        return evolutions.iterator();
    }

    /** Return the name of the application domain. */
    public String getDomain(String evName) { return domains.get(evName); }

    /** Get name of class to use for the Foundation trait. */
    public String getFoundation(String evName) { return foundations.get(evName); }

    /** Return all possible families. */
    @Override
    public Iterator<String> iterator() {
        return impls.iterator();
    }
}
