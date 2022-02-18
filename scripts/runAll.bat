@echo off
@REM run all tests

@REM move into proper directory
cd ..

set SAVED_JAVA_HOME=%JAVA_HOME%

for %%a in (oo visitor visitorSideEffect extensibleVisitor interpreter coco trivially dispatch) do (
  @echo off
  echo %%a
  echo %%a > scripts\jacoco.%%a

  echo Current time is %date% @ %time% >> scripts\jacoco.%%a

  @REM for each approach x model, execute to generate into target\ep2
  for %%e in (M0 J1 J2 J3 J4 J5 J6 J7 J8 J5J8 J9 J10) do (
     @echo off
     echo ====================================== >> scripts\jacoco.%%a
     echo %%e                                    >> scripts\jacoco.%%a
     echo ====================================== >> scripts\jacoco.%%a
     set JAVA_HOME=%SAVED_JAVA_HOME%

     sbt "language-java/runMain org.combinators.ep.language.java.DirectToDiskMainJ %%a %%e"

     @REM generated into target\ep2
     cd target\ep2
     zip -r ..\..\scripts\%%a-%%e-src.zip src
     
     set JAVA_HOME=C:\Program Files\AdoptOpenJDK\jdk-8.0.212.03-hotspot

     sbt jacoco            >> ..\..\scripts\jacoco.%%a
     echo                  >> ..\..\scripts\jacoco.%%a

     @REM back up to main
     cd ..\..
  )
)