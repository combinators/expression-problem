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

DIR         = f"."

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
PREFIX      = 'src\\main\\java'

def md5 (filename):
    """Return (# lines, md5) has for filename."""
    md5_hash = ''
    length = 0
    with open(filename, 'rb') as file:
        data = file.read()
        length = len(data.decode('ascii').split("\n"))
        md5_hash = hashlib.md5(data).hexdigest()
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
        return filename[loc + len(PREFIX) + 1:]
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
        eip_dirname = f"{DIR}\\{a}\\{eip}\\{PREFIX}"
        if not os.path.exists(eip_dirname):
            print(f"{eip_dirname} does not exist.")
            break

        if eip_dirname not in already_processed:
            eip_md5_data = extract(eip_dirname)
            already_processed[eip_dirname] = eip_md5_data
        else:
            eip_md5_data = already_processed[eip_dirname]

        for former_eip in pairs.get(eip):
            former_dirname = f"{DIR}\\{a}\\{former_eip}\\{PREFIX}"

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
