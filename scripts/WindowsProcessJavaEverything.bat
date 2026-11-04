@echo off
set "target=ep-java"
if exist "%target%\" (
  cd "%target%"
  c:\python314\python.exe ..\..\scripts\compile.py
  c:\Python314\python.exe ..\..\scripts\process.py > STATISTICS
  cd ..
)

set "target=ep-java-d1d2"
if exist "%target%\" (
  cd "%target%"
  c:\python314\python.exe ..\..\scripts\compile.py
  c:\Python314\python.exe ..\..\scripts\process.py > STATISTICS
  cd ..
)

set "target=ep-java-extended"
if exist "%target%\" (
  cd "%target%"
  c:\python314\python.exe ..\..\scripts\compile.py
  c:\Python314\python.exe ..\..\scripts\process.py > STATISTICS
  cd ..
)

set "target=ep-java-j"
if exist "%target%\" (
  cd "%target%"
  c:\python314\python.exe ..\..\scripts\compile.py
  c:\Python314\python.exe ..\..\scripts\process.py > STATISTICS
  cd ..
)

set "target=ep-java-merging"
if exist "%target%\" (
  cd "%target%"
  c:\python314\python.exe ..\..\scripts\compile.py
  c:\Python314\python.exe ..\..\scripts\process.py > STATISTICS
  cd ..
)

set "target=ep-java-third-alternate"
if exist "%target%\" (
  cd "%target%"
  c:\python314\python.exe ..\..\scripts\compile.py
  c:\Python314\python.exe ..\..\scripts\process.py > STATISTICS
  cd ..
)
