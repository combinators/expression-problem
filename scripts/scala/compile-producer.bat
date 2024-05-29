@echo off
cd ..\..

@REM First run GenerateAllProducer
@REM n Main which creates subdirectories in "target\ep-scala-producer"

@REM bring over the evolution specification for ease of use later
copy scripts\systems\system-producer.json target\ep-scala-producer
set TAB=..

for %%a in (oo extensibleVisitor interpreter coco algebra visitor visitorSideEffect trivially) do (
  @echo off
  echo %%a

  @REM for each approach x model, execute to generate into target\%DIR%
  for %%e in (M0 M1 M2 M3 W1 M3W1 Q1 C2 V1) do (
     echo %TAB% %%e
     call scripts\scala-compile.bat %%a %%e ep-scala-producer
  )
)

@REM So this script can be used by others, return to start where launched
cd target\ep-scala-producer
