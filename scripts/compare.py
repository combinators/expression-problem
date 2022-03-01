import hashlib
import zipfile

DIR = 'C:\\Users\\heineman\\Downloads\\CI\\expression-problem\\target\\analysis'

file = DIR + "\\" + 'coco-M0-src.zip'

approaches = 'oo visitor visitorSideEffect interpreter coco trivially dispatch'
evolutions = 'J1 J2 J3 K1 K2 J4 J5 J8 K2J6 J7 J8'

def file_structure (file):
    """Return dict of non-directory entries with hashlib entry."""
    zf = zipfile.ZipFile(file)
    index = {}
    for info in zf.infolist():
        if not info.is_dir():
            data = zf.read(info.filename)
            index[info.filename] = hashlib.md5(data).hexdigest()
    return index

# Make sure that the newly created version (key) is superset of older
# versions (values)
pairs = {
    'J1' : ['M0'],
    'J2' : ['J1'],
    'J3' : ['J2'],
    'J4' : ['J3'],
    'J5' : ['J4'],
    'K1' : ['J2'],
    'K2' : ['K1'],
    'J4' : ['J3'],
    'J5' : ['J4'],
    'J6' : ['J5'],
    'K2J6' : ['J6', 'K2'],
    'J7' : ['K2J6'],
    'J8' : ['J7']
}

for a in approaches.split():
    print('validating',a)
    for eip in evolutions.split():
        file = DIR + "\\" + a + "-" + eip + "-src.zip"
        new_structure = file_structure(file)
        for former in pairs.get(eip):
            old = DIR + "\\" + a + "-" + former + "-src.zip"
            old_structure = file_structure(old)

        different = None
        for f in old_structure:
            if different:
                break
            
            if (f in old_structure) and (not f in new_structure):
                different = f + ' appears in ' + former + ' but not in ' + eip
                break

            # if exists in both, consider fingerprint
            if old_structure[f] != new_structure[f]:
                different = f + ' has changed in ' + eip
                break

        if different:
            print('  ',a,'fails on',eip)
            
