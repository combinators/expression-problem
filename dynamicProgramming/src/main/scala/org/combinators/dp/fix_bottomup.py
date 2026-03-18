"""

I modified the fix_topdown to replace "int[]" with "Integer[]" throughout

MAKE SURE you do this only in a directory that was generated.

"""
import os

def process_java_file(path):
    with open(path, "r", encoding="utf-8") as f:
        lines = f.readlines()

    replaced = []
    updated = False
    for line in lines:
        if 'int[][]' in line:
            replaced.append(line.replace('int[][]', 'Integer[][]'))
            updated = True
        elif 'int[]' in line:
            replaced.append(line.replace('int[]', 'Integer[]'))
            updated = True
        else:
            replaced.append(line)

    if updated:
        print(f"Updated: {path}")
        with open(path, "w", encoding="utf-8") as f:
            f.writelines(replaced)

def process_directory(root_dir):
    for root, _, files in os.walk(root_dir):
        for file in files:
            if file.endswith(".java"):
                process_java_file(os.path.join(root, file))

if __name__ == "__main__":
    # Change this to your target subdirectory
    target_directory = os.getcwd()
    process_directory(target_directory)
