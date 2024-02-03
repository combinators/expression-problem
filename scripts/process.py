# Process jacoco.* output from runAll****.bat
#
# within the analysis/ directory that was result of runAll****.bat type followingL
#
#    python3 process.py

import sys
import glob
from scipy.stats import gmean

GMEAN = 'gmean'

# load up all files jacoco.*** and one of them HAS to be there, that is OO

oof = open('jacoco.oo')
ooA = oof.readlines()
oof.close()

def extract(A):
    """Extract information from the lines in the file."""
    printIt = 0
    results = []
    for line in A:
        if printIt > 0:
            if not '==' in line:
                results.append(line.strip())
        if '==' in line and printIt <= 0:
            printIt = 4

        printIt -= 1
    return results

def extract_modes_and_phases(results):
    modes = []
    phases = []
    timings = {}
    for x in range(0, len(results),2):
        dash = results[x].index('-')
        mode = results[x][:dash]
        phase = results[x][dash+1:]
        if phase not in phases:
            phases.append(phase)

        timings[results[x]] = int(results[x+1].split(',')[0])
        if not mode in modes:
            modes.append(mode)
    return (modes, phases, timings)

def extract_rows(modes, phases, timings):
    """Return dictionary of rows."""
    info = {}
    for m in modes:
        last = 0
        last_key = None
        for ph in phases:
            key = m + '-' + ph
            if last == 0:
                last = timings[key]
            else:
                elapsed = timings[key] - last
                info[last_key] = elapsed
                last = timings[key]
            last_key = key
    return info

results = extract(ooA)
modes,phases,timings = extract_modes_and_phases(results)
info = extract_rows(modes, phases, timings)

# Create table[mode][phase]
def create_table(modes, phases, info):
    tbl = {}
    tbl['header'] = {}
    for ph in phases[:-1]:
        tbl['header'][ph] = ph
        for m in modes:
            if not m in tbl:
                tbl[m] = {}
            tbl[m][ph] = info[m + '-' + ph]
    return tbl

oo_table = create_table(modes, phases, info)

def output_table(name, modes, phases, table):
    # Use OO  as baseline.
    print(name,',',','.join(phases[:-1]))
    for m in modes:
        print(m, end=',')
        for p in phases[:-1]:
            print('{0:.2f}'.format(table[m][p]), end=',')
        print()

    if GMEAN in table:
        m = GMEAN
        print(m, end=',')
        for p in phases[:-1]:
            print('{0:.2f}'.format(table[m][p]), end=',')
        print()

def output_gmeans_phase(AT, phase):
    print('phase', end=',')
    for key in AT:
        print(key, end=',')
    print()
    print(phase, end=',')
    for key in AT:
        print('{0:.2f}'.format(AT[key][GMEAN][p]), end=',')
    print()

def normalize_table(table, oo_tbl, modes, phases):
    for m in modes:
        for p in phases[:-1]:
            table[m][p] = table[m][p] / oo_tbl[m][p]

def add_geometric_mean(table, modes, phases):
    table[GMEAN] = {}
    for p in phases[:-1]:
        vals = []
        for m in modes:
            vals.append(table[m][p])
        table[GMEAN][p] = gmean(vals).item()    # Extract out python float32

filenamesList = glob.glob('jacoco.*')
all_tables = {}
all_tables['oo'] = oo_table

for name in filenamesList:
    f = open(name)
    A = f.readlines()
    f.close()
    results = extract(A)
    modes,phases,timings = extract_modes_and_phases(results)
    info = extract_rows(modes, phases, timings)
    table = create_table(modes, phases, info)

    normalize_table(table, oo_table, modes, phases)
    add_geometric_mean(table, modes, phases)

    descr = name.split('.')[1]
    all_tables[descr] = table

for key in all_tables:
    output_table(key, modes, phases, all_tables[key])

print()
for p in phases[:-1]:
    output_gmeans_phase(all_tables, p)
    print()