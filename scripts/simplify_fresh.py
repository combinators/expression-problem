# EpCoGen generates fresh names with a 32-character hexadecimal string
# like "exp_44e592962c164ae59f203c80430197a0" which has a leading "_".
# While this guarantees uniqueness, the resulting code can be hard to
# read. This script takes an entire generated codebase and replaces
# all these fresh names with a canonical ordering to greatly reduce
# the length of the lines and improve the readability of the code.

# Invoked like this:
#
#    python3 simplify_fresh.py SUFFIX
#
# where SUFFIX is typically "scala" or "java"

import sys
import os
import re

def fix_file(filename):
  """Fixes the fresh names for the given file."""

  with open(filename, 'rb') as file:
    data = file.read()

  fresh_index = 0
  fresh_ids = {}
  srcText = data.decode('ascii')
  regex = r"(_[a-f0-9]{32})"
  match = re.search(regex, srcText)
  while match:
    id = match.group(0)
    if not id in fresh_ids:
      fresh_index += 1
      fresh_ids[id] = f'_{fresh_index}'

    srcText = srcText[:match.start(0)] + fresh_ids[id] + srcText[match.end(0):]
    match = re.search(regex, srcText)

  with open(filename, 'wb') as file:
    file.write(srcText.encode('ascii'))

if len(sys.argv) < 2:
  print ("usage: python3 simplify_fresh.py SUFFIX")
  exit()

suffix = sys.argv[1]
for dirpath, _, filenames in os.walk('.'):
  for filename in filenames:
    if filename.endswith(suffix):
      file_path = os.path.join(dirpath, filename)
      fix_file(file_path)
      print('fixed', file_path, '...')
