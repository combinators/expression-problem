1. Generate all evolutions for all approaches

  1a. Execute runAllMain.bat, which computes code for all possible pairs, storing directories in target\analysis,
      as target\analysis\src-%%a-%%e where %%a is the approach and %%e is the evolution.

       Main File:    org.combinators.ep.language.java.DirectToDiskMain
       Evolutions:   M0 M1 M2 M3 M4 M5 M6 M7 M7I2 M8 M9 I1 A1 A1M3 A1M3I2 A3 I2
       Approaches:   oo visitor visitorSideEffect extensibleVisitor interpreter dispatch trivially coco algebra

  Note: Once done, move "target\analysis" to "target\analysis-Main" so it is not affected by following

  1b. Execute runAll.bat, which computes code for all possible pairs, storing directories in target\analysis,
      as target\analysis\src-%%a-%%e where %%a is the approach and %%e is the evolution.

       Main File:    org.combinators.ep.language.java.systemJ.DirectToDiskMainJ
       Evolutions:   M0 J1 J2 J3 K1 K2 J4 J5 J6 K2J6 J7 J8
       Approaches:   oo visitor visitorSideEffect extensibleVisitor interpreter dispatch trivially coco algebra

2. Produce full report to validate which AIPs are truly correct

  c:\Python37\python.exe ..\..\scripts\compare.py ..\..\scripts\[EVOLUTION-JSON] > REPORT

  where EVOLUTION-JSON is either "system-j.json" or "system-main.json". Note that the JSON files look like the following and contain
  predecessor information for each EIP to make the Python processing a bit easier.

         {
           "evolutions" : [
             { "J1" : ["M0"] },
             { "J2" : ["J1"] },
             ...
          }


  This script assesses whether an approach minimally satisfies the Expression Problem. That is, future evolutions
  do not require any changes to former artifacts.

  This script must be run in the directory that was generated from runAll.bat or a variant.

  Note that it does not identify situations where, for example, non-trivial code is copied and used in a subsequent
  evolution. For example, the 'interpreter' approach requires code to be copies with producer operations.

     // Within original M4 evolution
     // ------------------------------
     public Exp simplify() {
            if ((Double.valueOf(((Exp) this.left).eval()).equals(0.0) || Double.valueOf(((Exp) this.right).eval()).equals(0.0))) {
                return new Lit(0.0);
            } else if (Double.valueOf(((Exp) this.left).eval()).equals(1.0)) {
                return ((Exp) this.right).simplify();
            } else if (Double.valueOf(((Exp) this.right).eval()).equals(1.0)) {
                return ((Exp) this.left).simplify();
            } else {
                return new ep.m4.Mult(((Exp) this.left).simplify(), ((Exp) this.right).simplify());
            }
        }

     // Within original M7I2 evolution
     // ------------------------------
     public Exp simplify() {
         if ((Double.valueOf(((Exp) this.left).eval()).equals(0.0) || Double.valueOf(((Exp) this.right).eval()).equals(0.0))) {
             return new Lit(0.0);
         } else if (Double.valueOf(((Exp) this.left).eval()).equals(1.0)) {
             return ((Exp) this.right).simplify();
         } else if (Double.valueOf(((Exp) this.right).eval()).equals(1.0)) {
             return ((Exp) this.left).simplify();
         } else {
             return new ep.m7i2.Mult(((Exp) this.left).simplify(), ((Exp) this.right).simplify());
         }
     }

3. Generate statistics regarding the results of runAll.bat, which includes Generation time, Compilation time,
   and time to complete test cases. This script also detects errors in these three phases.

   CD into the directory in target\analysis and run the following on each of the jacoco.*** generated files

   c:\Python37\python.exe ..\..\scripts\process.py > STATISTICS

4. When all scripts have run, you will need to delete lots of temporary directories inside of target/bg-jobs that were created
   by SBT. A full run of the scripts often results in several GB worth of directories to be deleted.
