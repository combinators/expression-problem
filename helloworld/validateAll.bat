@REM validate all examples in helloWorld
@REM
@REM only works on windows machines
@REM
@REM Run this script within the top-level expression-problem directory

@echo off
setlocal

@REM Equality Exercises
for %%b in ("org.combinators.equals.EqualsJavaDirectToDiskMain" "org.combinators.equals.EqualsScalaDirectToDiskMain") do (
  echo sbt helloWorld/runMain %%~b

  call sbt "helloWorld/runMain %%~b"
  if %ERRORLEVEL% NEQ 0 (
      echo validateAll.bat failed on %%~b
      exit /b %ERRORLEVEL%
  )

  cd target\eql
  call sbt test
  if %ERRORLEVEL% NEQ 0 (
      echo validateAll.bat failed on %%~b
      exit /b %ERRORLEVEL%
  )
  cd ..\..
)


@REM Fibonacci Exercises
for %%b in ("org.combinators.fibonacci.FibonacciWithLucasJavaDirectToDiskMain" "org.combinators.fibonacci.FibonacciMainWithLucasScalaDirectToDiskMain"  "org.combinators.fibonacci.FibonacciMainScalaDirectToDiskMain" "org.combinators.fibonacci.FibonacciRecursiveVariableMainScalaDirectToDiskMain" "org.combinators.fibonacci.FibonacciRecursiveVariableMainScalaDirectToDiskMain imperative" "org.combinators.fibonacci.FibonacciRecursiveVariableMainScalaDirectToDiskMain functional" "org.combinators.fibonacci.FibonacciJavaDirectToDiskMain") do (
  echo sbt helloWorld/runMain %%~b

  call sbt "helloWorld/runMain %%~b"
  if %ERRORLEVEL% NEQ 0 (
      echo validateAll.bat failed on %%~b
      exit /b %ERRORLEVEL%
  )

  cd target\fib\scala
  call sbt test
  if %ERRORLEVEL% NEQ 0 (
      echo validateAll.bat failed on %%~b
      exit /b %ERRORLEVEL%
  )
  cd ..\..\..
)

endlocal
