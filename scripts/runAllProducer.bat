@echo off
@REM run all tests

@REM compiler code to compute time
javac Time.java

@REM move into proper directory and create target
cd ..

@REM directory in target/ where generated code is placed...
set DIR=ep3

@REM directory in target/ where analysis files will be generated
mkdir target\analysis

@REM: KEEP? set SAVED_JAVA_HOME=%JAVA_HOME%

for %%a in (oo visitor visitorSideEffect extensibleVisitor interpreter dispatch trivially coco algebra) do (
  @echo off
  echo %%a
  echo %%a > target\analysis\jacoco.%%a

  @REM for each approach x model, execute to generate into target\%DIR%
  for %%e in (M0 M1 M2 M3 W1 M3W1 Q1 C2 V1) do (
     @echo off
     echo ====================================== >> target\analysis\jacoco.%%a
     echo %%e-Generate                           >> target\analysis\jacoco.%%a
     java -cp scripts Time                       >> target\analysis\jacoco.%%a
     echo ====================================== >> target\analysis\jacoco.%%a
     @REM: KEEP? set JAVA_HOME=%SAVED_JAVA_HOME%

     sbt "language-java/runMain org.combinators.ep.language.java.DirectToDiskMainProducer %%a %%e"

     @REM generated into target\%DIR%
     cd target
     cd %DIR%
     xcopy src ..\analysis\src-%%a-%%e /E/H/C/I

     @REM: KEEP? set JAVA_HOME=C:\Users\heineman\Development\jdk-11.0.21+9

     @REM run Jacoco twice: the first time compiles. The second time only instruments
     @REM doesn't seem to be any way to avoid instrumentation in the second pass, but
     @REM at least this doesn't conflate initial compilation time
     echo ====================================== >> ..\analysis\jacoco.%%a
     echo %%e-Compile-Begin                      >> ..\analysis\jacoco.%%a
     java -cp ..\..\scripts Time                 >> ..\analysis\jacoco.%%a
     echo ====================================== >> ..\analysis\jacoco.%%a
     sbt jacoco            >> ..\analysis\jacoco.%%a

     echo ====================================== >> ..\analysis\jacoco.%%a
     echo %%e-Test-Begin                         >> ..\analysis\jacoco.%%a
     java -cp ..\..\scripts Time                 >> ..\analysis\jacoco.%%a
     echo ====================================== >> ..\analysis\jacoco.%%a

     sbt jacoco            >> ..\analysis\jacoco.%%a
     echo                  >> ..\analysis\jacoco.%%a

     echo ====================================== >> ..\analysis\jacoco.%%a
     echo %%e-Test-End                           >> ..\analysis\jacoco.%%a
     java -cp ..\..\scripts Time                 >> ..\analysis\jacoco.%%a
     echo ====================================== >> ..\analysis\jacoco.%%a

     @REM back up to main
     cd ..\..
  )
)
