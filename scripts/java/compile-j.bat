@echo off
cd ..\..

@REM First run GenerateAllJ
@REM in Main which creates subdirectories in "target\ep-java-j"

@REM bring over the evolution specification for ease of use later
copy scripts\systems\system-j.json target\ep-java-j
set TAB=..

for %%a in (oo extensibleVisitor interpreter coco algebra visitor visitorSideEffect dispatch trivially) do (
  @echo off
  echo %%a

  @REM for each approach x model, execute to generate into target\%DIR%
  for %%e in (M0 J1 J2 J3 K1 K2 J4 J5 J6 K2J6 J7 J8) do (
     echo %TAB% %%e
     call scripts\java-compile.bat %%a %%e ep-java-j
  )
)

@REM So this script can be used by others, return to start where launched
cd target\ep-java-j
