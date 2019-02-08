package make;

import java.util.ArrayList;
import java.util.Iterator;

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

        for (String p : priors) {
            Iterator<String> it = selfAndPast(p);
            while (it.hasNext()) {
                all.add(it.next());
            }
        }

        return all.iterator();
    }
}
