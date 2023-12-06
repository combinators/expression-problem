@echo off
@REM run all tests

@REM compiler code to compute time
javac Time.java

@REM move into proper directory and create target
cd ..

mkdir target\makeMain

for %%a in (oo visitor visitorSideEffect extensibleVisitor interpreter dispatch trivially coco algebra) do (

  @REM for each approach x model, execute to generate into target\ep2
  for %%e in (M0 M1 M2 M3 M4 M5 M6 M7 I1 I2 M7I2 M8 A1 A1M3 I2 A1M3I2 A3) do (
     @echo off

     @REM: You will need to remove " from the generated file
     echo language-java/runMain org.combinators.ep.language.java.DirectToDiskMain %%a %%e ">>" target\makeMain\outputgen >> target\makeMain\commands.sbt
  )
)

echo exit >> target\makeMain\commands.sbt

