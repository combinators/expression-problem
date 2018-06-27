package buildTest;

import java.util.*;
import java.net.*;
import java.io.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

/**
 * With new dynamic routes, we lose the information in the file.
 *
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

            // execute JUnit 3 test cases
            args = new String[] { "java" , "-cp",
                    junitJarFile + File.pathSeparator + ".",
                    "junit.textui.TestRunner",
                    pkgName + ".TestSuite"
            };
            proc = Runtime.getRuntime().exec(args, new String[0], dir);
            File outputFile = new File (new File (destination,pkgName), model + ".coverage.html");
            System.out.println(outputFile.getAbsoluteFile());
            PrintStream ps = new PrintStream(outputFile);
            err = new BufferedReader(new InputStreamReader(proc.getErrorStream())).lines();
            out = new BufferedReader(new InputStreamReader(proc.getInputStream())).lines();
            proc.waitFor();

            ps.println ("<h1>Errors (if any):</h1><font color='##0000'>"); ps.flush();
            out.forEach(line-> ps.println(line));
            ps.println ("</font><h1>Output</h1>");
            err.forEach(line -> ps.println(line)); ps.flush();
            ps.close();
            return true;
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
            System.err.println ("  Cannot find routes file:" + resources);
            System.exit(-1);
        }

        System.out.println ("Extracting all EP variations to:" + destination);

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
        for (String var : variations) {
            System.out.println ("Variation:" + var);
            String fields[] = var.split("/");

            // bit of a hack right now. fields[1] will be something like E3_Variation. We only want 'e3'
            gitRetrieve(fields[0], "e" + fields[1].charAt(1));
            compile(fields[0], "e" + fields[1].charAt(1));
        }
    }
}
