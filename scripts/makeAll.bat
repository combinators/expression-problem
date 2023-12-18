@REM move into proper directory and create target
cd ..

mkdir target\make

@REM: KEEP? set SAVED_JAVA_HOME=%JAVA_HOME%

for %%a in (interpreter oo visitor visitorSideEffect extensibleVisitor dispatch trivially coco algebra) do (
  @echo off
  echo %%a
  echo %%a > target\make\results.%%a

  @REM for each approach x model, execute to generate into target\ep2
  for %%e in (M0 M1 M2 M3 M4 M5 M6 M7 I1 I2 M7I2 M8) do (

     call :Generate %%a %%e
     echo %%a %%e %errorlevel%
  )
)
EXIT /B

:Generate
sbt "language-java/runMain org.combinators.ep.language.java.DirectToDiskMain %1 %2" >> target\make\results.%1
if "%ERRORLEVEL%"=="0" GOTO :good
echo %2-Generate-Fails                      >> target\make\results.%1
goto :last

:good
echo %2-Generate                           >> target\make\results.%1

cd target\ep3
sbt test
if "%ERRORLEVEL%"=="0" GOTO :goodcomp
echo %2-Compile-Fail                       >> target\make\results.%1
exit /B 1

:goodcomp
echo %2-Compile-Success                     >> target\make\results.%1

:backup
cd ..
cd ..
exit /B 0