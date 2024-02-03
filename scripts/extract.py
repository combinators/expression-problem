# extract info generated by runAll.bat IF directories generated are zip'd into zip files
import os

analysis_dir = os.path.join('..', 'target', 'analysis')

# 
approaches = 'oo visitor visitorSideEffect extensibleVisitor interpreter coco trivially dispatch'
evolutions = 'M0 J1 J2 J3 K1 K2 J4 J5 J8 K2J6 J7 J8'

gen_str = 'Generate'
test_str = 'Test'

meta_lines = 'Lines'
meta_instructions = 'Instructions'
meta_branches = 'Branches'
meta_methods = 'Methods'
meta_complexity = 'Complexity'
meta_class = 'Class'

def zipFileSize(aip, eip):
    zf = aip + "-" + eip + "-src.zip"
    actual = os.path.join(analysis_dir, zf)
    return os.path.getsize(actual)

table = {}
for aip in approaches.split():
    for eip in evolutions.split():
        sz = zipFileSize(aip,eip)
        fmt = f'{aip:20} {eip:10} {sz:10}'
        if not aip in table:
            table[aip]= {}
        table[aip][eip] = sz

atitle='AIP'
header = f'{atitle:20}'
for eip in evolutions.split():
    header += f'{eip:7} '

print (header)
for aip in approaches.split():
    row = f'{aip:20} '
    for eip in evolutions.split():
    
        sz = table[aip][eip]
        row += f'{sz:<7} '
    print(row)
    
# Now grab the times
tags = {}
times = {}
meta = {}
for aip in approaches.split():
    meta[aip] = {}
    filename = "jacoco." + aip
    times[aip] = {}
    with open(os.path.join(analysis_dir, filename)) as file:
        lines = [line.rstrip() for line in file]

    i = 0
    while i < len(lines):
       
        # extract JaCoCo information. Place before so we don't have to deal with bad i values
        if '[info] Lines:' in lines[i]:
            meta[aip][model][meta_lines] = lines[i].split(',')[1]
        elif '[info] Instructions:' in lines[i]:
            meta[aip][model][meta_instructions] = lines[i].split(',')[1]
        elif '[info] Branches:' in lines[i]:
            meta[aip][model][meta_branches] = lines[i].split(',')[1]
        elif '[info] Methods:' in lines[i]:
            meta[aip][model][meta_methods] = lines[i].split(',')[1]
        elif '[info] Complexity:' in lines[i]:
            meta[aip][model][meta_complexity] = lines[i].split(',')[1]
        elif '[info] Class:' in lines[i]:
            meta[aip][model][meta_class] = lines[i].split(',')[1]


        if '===============' in lines[i]:
            evolution = lines[i+1]
            model = evolution.split('-')[0]
            if not model in meta[aip]:
                meta[aip][model] = {}
            vals = lines[i+2].split(',')

            if not aip in tags:
                tags[aip]= {}
            tags[aip][evolution] = vals[0]
            i += 4
        else:
            i += 1

    for eip in evolutions.split():
        times[aip][eip] = {}
        gen_time = int(tags[aip][eip + '-Generate'])
        compile_begin = int(tags[aip][eip + '-Compile-Begin'])
        test_begin = int(tags[aip][eip + '-Test-Begin'])
        test_end = int(tags[aip][eip + '-Test-End'])
        times[aip][eip][gen_str] = compile_begin - gen_time
        times[aip][eip][test_str] = test_end - test_begin

    line =''
    line2 =''
    for eip in evolutions.split():
        gs = times[aip][eip][gen_str]
        ts = times[aip][eip][test_str]
        line += f'{gs:<7} '
        line2 += f'{ts:<7} '
    aipg=aip + '-G'
    aipt=aip + '-T'
    print(f'{aipg:20}', line)
    print(f'{aipt:20}', line2)

for e in [meta_lines, meta_instructions, meta_branches, meta_methods, meta_complexity, meta_class]:
    print(e)
    print('--------------------')
    for aip in approaches.split():
        aipg=aip + '-m'
        aipt=aip + '-M'
        row = f'{aipg:20}'
        row2 = f'{aipt:20}'
        for eip in evolutions.split():
            entry = meta[aip][eip][e].split(' ')
            row = f'{row}{entry[1]:<7}'
            row2 = f'{row2}{entry[3]:<7}'
            
        print(row)
        print(row2)


