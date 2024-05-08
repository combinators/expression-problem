@echo off
cd ..\..

@REM First run GenerateAllThirdAlternate in Main which creates subdirectories in "target\ep-java-third-alternate"

@REM bring over the evolution specification for ease of use later
copy scripts\systems\system-third-alternate.json target\java-third-alternate
set TAB=..

for %%a in (oo extensibleVisitor interpreter coco algebra visitor visitorSideEffect dispatch trivially) do (
  @echo off
  echo %%a

  @REM for each approach x model, execute to generate into target\%DIR%
  for %%e in (M0 X1 X2 X3 X2X3 X4) do (
     echo %TAB% %%e
     call scripts\java-compile.bat %%a %%e ep-java-third-alternate
  )
)


@REM So this script can be used by others, return to start where launched
cd target\ep-java-third-alternate