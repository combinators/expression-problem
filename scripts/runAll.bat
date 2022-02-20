@echo off
@REM run all tests

@REM compiler code to compute time
javac Time.java

@REM move into proper directory and create target
cd ..

mkdir target\analysis

@REM burn one run, to ensure no bias from "first one run"
sbt "language-java/runMain org.combinators.ep.language.java.DirectToDiskMainJ coco j10"

set SAVED_JAVA_HOME=%JAVA_HOME%

for %%a in (oo visitor visitorSideEffect extensibleVisitor interpreter dispatch trivially coco) do (
  @echo off
  echo %%a
  echo %%a > target\analysis\jacoco.%%a

  @REM for each approach x model, execute to generate into target\ep2
  for %%e in (M0 J1 J2 J3 J4 J5 J6 J7 J8 J5J8 J9 J10) do (
     @echo off
     echo ====================================== >> target\analysis\jacoco.%%a
     echo %%e-Generate                           >> target\analysis\jacoco.%%a
     java -cp scripts Time                       >> target\analysis\jacoco.%%a
     echo ====================================== >> target\analysis\jacoco.%%a
     set JAVA_HOME=%SAVED_JAVA_HOME%

     sbt "language-java/runMain org.combinators.ep.language.java.DirectToDiskMainJ %%a %%e"

     @REM generated into target\ep2
     cd target\ep2
     zip -qr ..\analysis\%%a-%%e-src.zip src
     
     set JAVA_HOME=C:\Program Files\AdoptOpenJDK\jdk-8.0.212.03-hotspot
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
