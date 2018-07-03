package buildTest;

import java.util.*;
import java.io.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

/**
 * With new dynamic routes, we lose the information in the file.
 */
public class TestSynthesis {

    public static final String resources = "src" + File.separator + "main" + File.separator + "resources" + File.separator + "routes";

    /** All synthesized files are stored in demo folder. */
    public static final String destination = "demo" + File.separator + "ep";

    /** Timing helpers. */
    static long start_timeStamp;
    static void startTime() {
        start_timeStamp = System.currentTimeMillis();
    }
    static float endTime() {
        float seconds = (1.0f*(System.currentTimeMillis() - start_timeStamp) / 1000);
        return seconds;
    }

    /**
     * Use git clone to retrieve the source files and move to destination/family/model
     *
     * @return true on success. false otherwise
     */
    static boolean gitRetrieve(String family, String model) {
        String url = String.format("http://localhost:9000/%s/%s/%s.git", family, model, model);
        File dir = new File (destination);
        if (!dir.exists() && !dir.mkdir()) {
            System.err.println ("  unable to make directory:" + destination);
            return false;
        }

        // Make family folder (if not yet there)
        File child = new File (dir, family);
        if (!child.exists() && !child.mkdir()) {
            System.err.println ("  unable to make directory:" + dir + File.separatorChar + family);
            return false;
        }
        // git clone -n variation_0 http://localhost:9000/freecell/doublefreecell/doublefreecell.git
        String command = "git clone -b variation_0 " + url;
        try {
            Process proc = Runtime.getRuntime().exec(command, new String[0], child);
            System.out.println ("  Errors (if any):"); System.out.flush();
            Stream<String> err = new BufferedReader(new InputStreamReader(proc.getErrorStream())).lines();
            err.forEach(System.err::println); System.err.flush();
            System.out.println ("  Output (if any):"); System.out.flush();
            Stream<String> out = new BufferedReader(new InputStreamReader(proc.getInputStream())).lines();
            out.forEach(System.out::println);
            System.out.println ("  ----"); System.out.flush();
            proc.waitFor();
            return true;
        } catch (Exception e) {
            System.err.println ("  Unable to exec:" + command);
            return false;
        }
    }

    /**
     * Report on changes
     *
     * A    file.java   [added]
     *
     * @param family
     * @param newer
     * @param older
     * @return
     */
    static Iterator<String> compare(String family, String newer, String older) {
        File dirNew = new File(new File(destination, family), newer);
        File dirOld = new File(new File(destination, family), older);

        // for every file in 'dirNew' check if exists in dirOld, and if changed.
        // TBA
        return new ArrayList<String>().iterator();
    }

    /**
     * Compile the classes as found in the given location
     *
     * @return true on success; false otherwise
     */
    static boolean compile(String family, String model) {
        File here = new File (".");
        String junitJarFile = here.getAbsoluteFile().toString() + File.separatorChar + destination + File.separatorChar + "junit.jar";

        File dir = new File (destination, family);
        if (!dir.exists()) {
            System.err.println ("  unable to locate destination directory:" + destination + File.separator + family);
            return false;
        }
        dir = new File (dir, model);
        if (!dir.exists()) {
            System.err.println ("  unable to locate destination directory:" + destination + File.separator + model);
            return false;
        }
        dir = new File (dir, "src");
        dir = new File (dir, "main");
        dir = new File (dir, "java");

        // find directory name
        File children[] = dir.listFiles();
        String pkgName = "";
        if (children == null || children.length == 0) {
            pkgName = ".";
        } else {
            File onlyChild = null;
            for (File child : children) {
                if (!child.getName().startsWith(".")) {
                    onlyChild = child;
                }
            }
            pkgName = onlyChild.getName();
        }

        // javac -cp standAlone.jar:./bigforty/src/main/java klondike/src/main/java/org/combinators/solitaire/bigforty/BigForty.java
        //
        //run-bigforty: bigforty
        //        java -cp standAlone.jar:./bigforty/src/main/java org/combinators/solitaire/bigforty/BigForty
        String fs = File.separator;
        String[] args = new String[] { "javac",  "-cp",
                junitJarFile + File.pathSeparator + ".",
                //"-Xlint:unchecked",
                pkgName + fs + "*.java"};

        try {
            Process proc = Runtime.getRuntime().exec(args, new String[0], dir);

            System.out.println ("  Errors (if any):"); System.out.flush();
            Stream<String> err = new BufferedReader(new InputStreamReader(proc.getErrorStream())).lines();
            err.forEach(System.err::println); System.err.flush();
            System.out.println ("  Output (if any):"); System.out.flush();
            Stream<String> out = new BufferedReader(new InputStreamReader(proc.getInputStream())).lines();
            out.forEach(System.out::println);
            System.out.println ("  ----"); System.out.flush();
            proc.waitFor();

            if (proc.exitValue() == 0) {

                // execute JUnit 3 test cases
                args = new String[]{"java", "-cp",
                        junitJarFile + File.pathSeparator + ".",
                        "junit.textui.TestRunner",
                        pkgName + ".TestSuite"
                };
                proc = Runtime.getRuntime().exec(args, new String[0], dir);
                File outputFile = new File(new File(destination, family), model + ".coverage.html");
                System.out.println(outputFile.getAbsoluteFile());
                PrintStream ps = new PrintStream(outputFile);
                err = new BufferedReader(new InputStreamReader(proc.getErrorStream())).lines();
                out = new BufferedReader(new InputStreamReader(proc.getInputStream())).lines();
                proc.waitFor();
                int retVal = proc.exitValue();

                ps.println("<h1>Errors (if any):</h1><font color='##0000'>");
                ps.flush();
                out.forEach(line -> ps.println(line));
                ps.println("</font><h1>Output</h1>");
                err.forEach(line -> ps.println(line));
                ps.flush();
                ps.close();
                return retVal == 0;
            } else {
                return false;
            }
        } catch (Exception e) {
            System.err.println ("  Unable to exec:" + Arrays.toString(args));
            return false;
        }
    }


    /**
     * Launch everything!
     *
     * All code is stored in nextgen-solitaire/demo/solitaire and can be deleted at any time
     * since the generated code is not part of the git repository.
     */
    public static void main (String args[]) throws Exception {

        File f = new File(resources);
        if (!f.exists()) {
            System.err.println("  Cannot find routes file:" + resources);
            System.exit(-1);
        }

        System.out.println("Extracting all EP variations to:" + destination);

        ArrayList<String> variations = new ArrayList<>();
        Scanner sc = new Scanner(f);
        while (sc.hasNextLine()) {
            String s = sc.nextLine();

            Pattern regex = Pattern.compile("->\\s+/\\s+example\\.expression\\.(\\w+)\\.(\\w+)");
            Matcher match = regex.matcher(s);

            if (match.find()) {
                String family = match.group(1);
                String evolutionID = match.group(2);
                variations.add(family + "/" + evolutionID);
            }
        }

        Collections.shuffle(variations);

        // Perform each one in random order, so we can run multiple trials
        // i.e., algebra -> {(s -> s-family), (e -> e-family) }
        Hashtable<String,Hashtable<String,Family>> successful = new Hashtable<>();
        for (String var : variations) {
            System.out.println("Variation:" + var);

            String fields[] = var.split("/");
            String family = fields[0];

            // bit of a hack right now. fields[1] will be something like E3_Variation or S1_Variation.
            // We only want 'e3' or 's3'.
            // keep track of all successful variations, as well as highest id for each one.
            int underscore = fields[1].indexOf('_');
            if (underscore == -1) { underscore = 2; }
            int id = Integer.valueOf(fields[1].substring(1,underscore));  // only grab digits UP TO _
            String prefix = fields[1].toLowerCase().substring(0,1);

            String variation = prefix + id;
            if (gitRetrieve(family, variation)) {
                if (compile(family, variation)) {

                    // create Hashtable<String,Family> if not already there...
                    if (!successful.containsKey(family)) {
                        successful.put(family, new Hashtable<>());
                        System.out.println ("** CREATE FAMILY " + family + "**");
                    }
                    Hashtable<String,Family> fam = successful.get(family);

                    // for the given family, see if prefix exists. Add if not already there...
                    if (!fam.containsKey(prefix)) {
                        fam.put(prefix, new Family(prefix));
                        System.out.println ("** CREATE prefix " + prefix + "**");
                    }

                    // add successful variation to this prefix for given family.
                    Family pf = fam.get(prefix);
                    pf.add(id);
                    System.out.println ("** Add id " + family + ":" + prefix + " **");

                } else {
                    System.err.println ("RETRIEVED " + family + variation + " but failed to compile.");
                }
            }
        }

        // compare differences with each family and prefix, starting with highest known ID and working backwards
        for (String family : successful.keySet()) {
            System.out.println("Family: " + family);

            Hashtable<String, Family> ht = successful.get(family);

            for (String prefix : ht.keySet()) {
                System.out.print (" prefix-" + prefix + " ");
                for (String var : ht.get(prefix)) {
                    System.out.print(var + " ");
                }
            }

// Use these commands to determine what code changed
//            git clone -b variation_0 http://localhost:9000/algebra/e5/e5.git
//            cd e5
//            git remote add -f b http://localhost:9000/algebra/e4/e4.git
//            git remote update
//            git diff remotes/b/variation_0 variation_0  --diff-filter=A --name-only
//            git diff remotes/b/variation_0 variation_0  --diff-filter=M --name-only
//            git diff remotes/b/variation_0 variation_0  --diff-filter=D --name-only



            System.out.println();
        }
    }
}
