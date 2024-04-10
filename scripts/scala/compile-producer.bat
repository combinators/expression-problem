@echo off
cd ..\..

@REM First run GenerateAllProducer in Main which creates subdirectories in "target\ep-all-producer"

@REM directory in target/ where analysis files will be generated
mkdir target\analysis

@REM bring over the evolution specification for ease of use later
copy scripts\systems\system-producer.json target\analysis
set TAB=..

for %%a in (oo extensibleVisitor interpreter coco algebra visitor visitorSideEffect trivially) do (
  @echo off
  echo %%a

  @REM for each approach x model, execute to generate into target\%DIR%
  for %%e in (M0 M1 M2 M3 W1 M3W1 Q1 C2 V1) do (
     echo %TAB% %%e
     call scripts\scala-compile.bat %%a %%e ep-all-producer
  )
)
