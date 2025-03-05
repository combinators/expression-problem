"""
    Execute this Python script from within the ep-java-XXX or ep-scala-XXX generated 
    directory and it will compile, test, and run code coverage for all approaches and stages.

"""
import os
import platform
from os import listdir, path
from os.path import isdir, join, realpath
import subprocess
import time
from datetime import datetime

has_shell = (platform.system() == 'Windows')
JAVA = 'java'
SCALA = 'scala'

def java_or_scala():
   """Assuming running in ep-java-XXX or ep-scala-XXX directory, return 'java' or 'scala'."""
   cwd = os.getcwd()
   approaches = os.listdir(cwd)
   one = approaches[0]
   stages = os.listdir(path.join(cwd, one))
   print(stages[0])
   stage = stages[0]
   languages = os.listdir(path.join(cwd,one,stage,'src','main'))
   if JAVA in languages:
       return JAVA
   if SCALA in languages:
       return SCALA
   return None

# only difference by language
java_coverage = ['sbt', 'jacoco']
scala_coverage = ['sbt', 'coverageReport']
which = java_or_scala()
if which == JAVA:
    coverage = java_coverage
    print('Processing JAVA source directory')
elif which == SCALA:
    coverage = scala_coverage
    print('Processing SCALA source directory')
else:
    print('Cannot determine Java or Scala language. Are you running this script in proper directory?')
    exit(1)

# since we are running inside the ep-java-XXX directory, 

def current_milli_time():
    return round(time.time() * 1000)

tzone = time.localtime().tm_zone
fmt = f'%a %b %d %H:%M:%S {tzone} %Y'

def timestamp(output):
    dt = datetime.now()
    output.write(f'{current_milli_time()},{dt.strftime(fmt)}\n')

# Assume run in the ep-java-XXX or ep-scala-XXX directory which is in target; therefore, 
# go up two directories and down into 'scripts' to find necessary script files.
cwd = os.getcwd()
scripts_dir = path.join(cwd, '..', '..', 'scripts')

approaches = [f for f in listdir('.') if isdir(join('.', f))]
for app in approaches:
    log = open(f'jacoco.{app}', 'a')
    stages = [f for f in listdir(app) if isdir(join(app, f))]
    print(app, ','.join(stages))

    for stage in stages:
        print(app, stage)
        dir = path.join(cwd, app, stage)

        log.write('======================================\n')
        log.write(f'{stage}-Compile-Begin                \n')
        timestamp(log)
        log.write('======================================\n')
        log.flush()

        subprocess.run(['sbt', 'compile'], cwd=dir, shell=has_shell, stdout=log, stderr=log)

        log.write('======================================\n')
        log.write(f'{stage}-Test-Begin                   \n')
        timestamp(log)
        log.write('======================================\n')
        log.flush()

        subprocess.run(['sbt', 'test'], cwd=dir, shell=has_shell, stdout=log, stderr=log)

        log.write('======================================\n')
        log.write(f'{stage}-Test-End                     \n')
        timestamp(log)
        log.write('======================================\n')
        log.flush()

        subprocess.run(coverage, cwd=dir, shell=has_shell, stdout=log, stderr=log)

    log.close()
