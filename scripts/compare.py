"""
  Run with Python3 to generate report of approaches that satisfy EP criteria.

  python3 ..\..\scripts\compare.py ..\..\scripts\systems\[EVOLUTION-JSON] >> REPORT

  where the JSON file has a single 'evolutions' tag, where each entry

    {
      "directory" : "analysis-J",
      "evolutions" : [
        { "J1" : ["M0"] },
        { "J2" : ["J1"] },
        { "J3" : ["J2"] },
         ...
      ]
    }

  It determines the language (scala or java) by inspecting directory names.

  Depends on having successfully generated files as outlined in the README.txt file.

  Currently only checks that OLD files are not changed from one evolution to the next.
  Does not check that non-trivia code is copied from one iteration to the next (which can
  happen, especially with producer methods. See Interpreter solution for examples of large
  chunks of copied code.

  In addition, there are some EP approaches that can reuse test cases, but in others, even the
  test cases have to be altered.
"""
import hashlib
import os
import zipfile
import json
import sys
import re
DIR = f"."

JAVA = 'java'
SCALA = 'scala'

def java_or_scala():
   """Assuming running in ep-java-XXX or ep-scala-XXX directory, return 'java' or 'scala'."""
   cwd = os.getcwd()
   approaches = os.listdir(cwd)
   one = approaches[0]
   stages = os.listdir(os.path.join(cwd, one))
   print(stages[0])
   stage = stages[0]
   languages = os.listdir(os.path.join(cwd,one,stage,'src','main'))
   if JAVA in languages:
       return JAVA
   if SCALA in languages:
       return SCALA
   return None

from os.path import isdir, join
approaches = [f for f in os.listdir(DIR) if os.path.isdir(os.path.join(DIR, f))]

if len(sys.argv) <= 1:
    print ("Usage: python3 compare.py JSON-FILE")
    print ("  Execute this command within the target/analysis directory where all subdirectories were generated.")
    exit (0)

# Load up description of system
description = None
with open(sys.argv[1], 'r') as file:
    description = json.load(file)

# Choose evolutions to generate
evolutions = []
pairs = {}
for k in description["evolutions"]:
    key = list(k.keys())[0]
    vals = k[key]
    pairs[key] = vals
    evolutions.append(list(k.keys())[0])

# Want to be able to detect differences in the actual source code. Note that test cases are a
# different story, and even solutions to the EP require special handling with regard to test cases
fix_fresh = False
lang = java_or_scala()

if lang == 'java':
    print('Comparing Java source directories...')
    src_dir = os.path.join('src', 'main', JAVA)
elif lang == 'scala':
    print('Comparing Scala source directories...')
    src_dir = os.path.join('src', 'main', SCALA)
    fix_fresh = True
else:
    print("Cannot determine language of source code directory.")
    exit (1)
PREFIX = str(src_dir)

def md5 (filename):
    """Return (# lines, md5) has for filename."""
    md5_hash = ''
    length = 0
    fresh_index = 0
    with open(filename, 'rb') as file:
        data = file.read()
        fresh_ids = {}
        srcText = data.decode('ascii')
        if fix_fresh:
            """Replaces all fresh id's canonically to offer valid comparison."""
            regex = r"(_[a-f0-9]{32})"
            match = re.search(regex, srcText)
            while match:
                id = match.group(0)
                if not id in fresh_ids:
                    fresh_index += 1
                    fresh_ids[id] = f'{fresh_index}'

                srcText = srcText[:match.start(0)] + fresh_ids[id] + srcText[match.end(0):]
                match = re.search(regex, srcText)

        length = len(srcText.split("\n"))
        md5_hash = hashlib.md5(srcText.encode()).hexdigest()
    return (length, md5_hash)

# If source directory were zip'd, this would be useful.
def file_structure (file):
    """Return dict of non-directory entries with hashlib entry."""
    zf = zipfile.ZipFile(file)
    index = {}
    for info in zf.infolist():
        if not info.is_dir():
            data = zf.read(info.filename)
            index[info.filename] = hashlib.md5(data).hexdigest()
    return index

def strip_prefix(filename):
    """Remove PREFIX\ from the filename."""
    if PREFIX in filename:
        loc = filename.index(PREFIX)
        reduced = filename[loc + len(PREFIX) + 1:]
        return reduced
    return filename

def extract(eip_dirname):
    """Return md5 data for the entire directory."""
    eip_walk = os.walk(eip_dirname, topdown=True)
    md5_data = {}

    # store everything about eip
    for root, dirs, files in eip_walk:
        for name in files:
            filename = os.path.join(root, name)
            key = strip_prefix(filename)
            md5_data[key] = md5(filename)
    return md5_data

for a in approaches:
    print('validating',a)

    already_processed = {}
    for eip in evolutions:
        eip_dirname = os.path.join(DIR, a, eip, PREFIX)
        if not os.path.exists(eip_dirname):
            print(f"{eip_dirname} does not exist.")
            break

        if eip_dirname not in already_processed:
            eip_md5_data = extract(eip_dirname)
            already_processed[eip_dirname] = eip_md5_data
        else:
            eip_md5_data = already_processed[eip_dirname]

        for former_eip in pairs.get(eip):
            former_dirname = os.path.join(DIR, a, former_eip, PREFIX)

            if former_dirname not in already_processed:
                former_md5_data = extract(former_dirname)
                already_processed[former_dirname] = former_md5_data
            else:
                former_md5_data = already_processed[former_dirname]

            print (f" compare {eip_dirname} with {former_dirname}")

            different = None
            for f in former_md5_data:
                if different:
                    break

                # get data for both
                former_length, former_md5_hash = former_md5_data[f]

                # IF exists in the OLD generation BUT not the new one, it is a problem
                if not f in eip_md5_data:
                    different = f"{f} appears in {former_eip} for {a} but not in {eip}."
                    break

                # so it exists in BOTH, consider fingerprint
                eip_length, eip_md_hash = eip_md5_data[f]

                if former_md5_hash != eip_md_hash:
                    different = f"{f} has changed in {eip} for {a} compared to {former_eip}."
                    break

            if different:
                print(f"  {a} fails on {eip}: {different}")
