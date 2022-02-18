@echo off
@REM run all tests

@REM move into proper directory
cd ..

set SAVED_JAVA_HOME=%JAVA_HOME%

for %%a in (oo visitor visitorSideEffect extensibleVisitor interpreter coco trivially dispatch) do (
  echo %%a
  echo %%a > jacoco.%%a

  for /F "tokens=2" %%i in ('date /t') do set mydate=%%i
  set mytime=%time%
  echo Current time is %mydate%:%mytime% >> jacoco.%%a

  @REM for each approach x model, execute to generate into target\ep2
  for %%e in (M0 J1 J2 J3 J4 J5 J6 J7 J8 J5J8 J9 J10) do (
     set JAVA_HOME=%SAVED_JAVA_HOME%

     sbt "language-java/runMain org.combinators.ep.language.java.DirectToDiskMainJ %%a %%e"

     @REM generated into target\ep2
     cd target\ep2
     zip -r ..\%%a-%%e-src.zip src
     echo %%e >> jacoco.%%a
     set JAVA_HOME=C:\Program Files\AdoptOpenJDK\jdk-8.0.212.03-hotspot

     sbt jacoco >> jacoco.%%a
     echo >> jacoco.%%a

     @REM back up to main
     cd ..\..
  )
)