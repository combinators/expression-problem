"""
    Execute this Python script from within the expression-problem git repository
    and it will compile, test and run code coverage for a Quick Validation on the
    most complicated systems.

    Exits (0) if no errors, otherwise (1) for problems in either Java or Scala

"""
import os
import sys
import platform
from os import listdir, path
from os.path import isdir, join, realpath
import subprocess
import time
from datetime import datetime

has_shell = (platform.system() == 'Windows')

def current_milli_time():
        return round(time.time() * 1000)

tzone = time.localtime().tm_zone
fmt = f"%a %b %d %H:%M:%S {tzone} %Y"

def header(s):
    log.write('======================================\n')
    log.write(f'{s}\n')
    timestamp(log)
    log.write('======================================\n')
    print(s, datetime.now())

def timestamp(output):
    dt = datetime.now()
    output.write(f'{current_milli_time()},{dt.strftime(fmt)}\n')

# Assume run in the expression-target
dir = os.getcwd()
scripts_dir = path.join(dir, 'scripts')
compile = path.join(dir, 'scripts', 'compile.py')

process = path.join(dir, 'scripts', 'process.py')

java_dir = path.join(dir, 'target', 'ep-java-quick')
scala_dir = path.join(dir, 'target', 'ep-scala-quick')

log = open('quick-validation.txt', 'a')

# ----------------------- Scala -----------------------------------
header('Scala-Generate-Quick-Begin')
subprocess.run(['sbt', 'language-newScala/runMain org.combinators.ep.language.scala.codegen.QuickValidation'], cwd=dir, shell=has_shell, stdout=log, stderr=log)

header('Scala-Generate-Quick-Compile-Begin')
subprocess.run([sys.executable, compile], cwd=scala_dir, shell=has_shell, stdout=log, stderr=log)

header('Scala-Generate-Quick-Compile-End')

STATS_scala = path.join(scala_dir, 'STATISTICS')
log_stat = open(STATS_scala, 'a')
subprocess.run([sys.executable, process], cwd=scala_dir, shell=has_shell, stdout=log_stat, stderr=log_stat)
log_stat.close()
# ----------------------- Scala End -----------------------------------

# ----------------------- Java -----------------------------------
header('Java-Generate-Quick-Begin')
subprocess.run(['sbt', 'language-java/runMain org.combinators.ep.language.java.QuickValidation'], cwd=dir, shell=has_shell, stdout=log, stderr=log)

header('Java-Generate-Quick-Compile-Begin')
subprocess.run([sys.executable, compile], cwd=java_dir, shell=has_shell, stdout=log, stderr=log)

header('Java-Generate-Quick-Compile-End')

STATS_java = path.join(java_dir, 'STATISTICS')
log_stat = open(STATS_java, 'a')
subprocess.run([sys.executable, process], cwd=java_dir, shell=has_shell, stdout=log_stat, stderr=log_stat)
log_stat.close()
# ----------------------- Java End -----------------------------------

def contains_error(filename):
    # check if error appears anywhere in 'STATISTICS' files (either java or scala)
    uc = open(STATS_scala, 'r', encoding='utf-8')
    names = uc.readlines()
    uc.close()

    s = '\n'.join(names)
    return 'error' in s

java_failed = contains_error(STATS_java)
scala_failed = contains_error(STATS_scala)

log.write('\n')
log.write(f'Java Failed: {java_failed}\n')
log.write(f'Scala Failed: {scala_failed}\n')
log.close()

if java_failed or scala_failed:
    print(f'Java Failed: {java_failed} Scala Failed: {scala_failed}')
    exit(1)

print('No errors!')
exit(0)
