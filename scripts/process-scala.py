# Process jacoco.* output from compile-XXX.bat files for Scala
#
# within the analysis/ directory that was result of runAll****.bat type followingL
#
#    python3 ..\..\scripts\process-scala.py

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
    hasError = ''
    errors = []
    coverages = []
    coverage_instructions = coverage_branches = ''
    for line in A:
        if '[error]' in line:
            hasError = 'error'
            continue
        if '[info]' in line and 'Statement Coverage' in line:
            coverage_instructions = line.split()[2]
        if '[info]' in line and 'Branch Coverage' in line:
            coverage_branches = line.split()[2]
            coverages.append(f'{coverage_instructions},{coverage_branches}')
            coverage_instructions = coverage_branches = ''
        if printIt > 0:
            if not '==' in line:
                results.append(line.strip())
                errors.append(hasError)
                hasError = ''
        if '==' in line and printIt <= 0:
            printIt = 4

        printIt -= 1
    return (results,errors,coverages)

def extract_modes_and_phases(results, in_errors):
    modes = []
    phases = []
    timings = {}
    errs = {}
    for x in range(0, len(results),2):
        if '-' not in results[x]:
            print('skip:', results[x])
            continue
        dash = results[x].index('-')
        mode = results[x][:dash]
        phase = results[x][dash+1:]
        if phase not in phases:
            phases.append(phase)

        timings[results[x]] = int(results[x+1].split(',')[0])
        errs[results[x]] = in_errors[x]
        if not mode in modes:
            modes.append(mode)
    return (modes, phases, timings, errs)

def extract_rows(modes, phases, timings):
    """Return dictionary of rows."""
    info = {}
    for m in modes:
        last = 0
        last_key = None
        for ph in phases:
            key = m + '-' + ph
            if key not in timings:
                continue
            if last == 0:
                last = timings[key]
            else:
                elapsed = timings[key] - last
                info[last_key] = elapsed
                last = timings[key]
            last_key = key
    return info

results,errors,oo_cov_table = extract(ooA)
modes,phases,timings,errors = extract_modes_and_phases(results, errors)
info = extract_rows(modes, phases, timings)

# Create table[mode][phase]
def create_table(modes, phases, info, errors):
    tbl = {}
    err = {}
    tbl['header'] = {}
    err['header'] = {}
    for ph in phases[:-1]:
        tbl['header'][ph] = ph
        err['header'][ph] = ph
        for m in modes:
            if not m in tbl:
                tbl[m] = {}
                err[m] = {}
            tbl[m][ph] = info[m + '-' + ph]
            err[m][ph] = errors[m + '-' + ph]
    return (tbl,err)

oo_table,oo_err_table = create_table(modes, phases, info, errors)

def output_table(name, modes, phases, table, cov_table, errors):
    # Use OO  as baseline.
    print(name,',',','.join(phases[:-1]))
    for m in modes:
        print(m, end=',')
        for p in phases[:-1]:
            print('{0:.2f}'.format(table[m][p]), end=',')
        for p in phases[:-1]:
            print(errors[m][p], end=',')
        if cov_table:
            print(cov_table[0], end=',')
            cov_table = cov_table[1:]
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
        if m in table:
            for p in phases[:-1]:
                if p in table[m]:
                    table[m][p] = table[m][p] / oo_tbl[m][p]
                else:
                    table[m][p] = 0

def add_geometric_mean(table, modes, phases):
    table[GMEAN] = {}
    for p in phases[:-1]:
        vals = []
        for m in modes:
            vals.append(table[m][p])
        table[GMEAN][p] = gmean(vals).item()    # Extract out python float32

filenamesList = glob.glob('jacoco.*')
all_tables = {}
err_tables = {}
cov_tables = {}
all_tables['oo'] = oo_table
err_tables['oo'] = oo_err_table
cov_tables['oo'] = oo_cov_table

for name in filenamesList:
    f = open(name)
    A = f.readlines()
    f.close()
    results,errors,coverages = extract(A)
    modes,phases,timings,errors = extract_modes_and_phases(results, errors)
    info = extract_rows(modes, phases, timings)

    table,err_table = create_table(modes, phases, info, errors)

    normalize_table(table, oo_table, modes, phases)
    add_geometric_mean(table, modes, phases)

    descr = name.split('.')[1]
    all_tables[descr] = table
    err_tables[descr] = err_table
    cov_tables[descr] = coverages

for key in all_tables:
    output_table(key, modes, phases, all_tables[key], cov_tables[key], err_tables[key])

print()
for p in phases[:-1]:
    output_gmeans_phase(all_tables, p)
    print()