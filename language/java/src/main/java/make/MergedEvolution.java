package make;

import java.util.ArrayList;
import java.util.Iterator;

/**
 * Handles the merged evolution cases.
 */
public class MergedEvolution extends Evolution {
    final String[] priors;

    public MergedEvolution(String name, String... priors) {
        super(name);

        int num = priors.length;
        this.priors = new String[num];
        for (int i = 0; i < num; i++) {
            this.priors[i] = priors[i];
        }
    }

    boolean isSimple() { return false; }

    // prior evolutions (assumes no cycles!)
    public Iterator<String> evolutions() {
        ArrayList<String> all = new ArrayList<>();

        // don't forget to add self AS FIRST one
        all.add(name);

        // place in order. If we revisit one already existing, then delete OLD one
        for (String p : priors) {
            Iterator<String> it = selfAndPast(p);
            while (it.hasNext()) {
                String ev = it.next();
                if (all.contains(ev)) {
                    all.remove(ev);
                }
                all.add(ev);
            }
        }

        return all.iterator();
    }
}
