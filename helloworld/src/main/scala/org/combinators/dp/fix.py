"""
I got the following script from gpt using this prompt:

'can you give me a python script that adds "import java.util.*" to the beginning of all
Java files in a subdirectory, paying careful attention to do so after the "package" declaration.'

Not too shabby. I only modified the final line to use os.getcwd()

MAKE SURE you do this only in a directory that was generated.

"""
import os

IMPORT_LINE = "import java.util.*;\n"

def process_java_file(path):
    with open(path, "r", encoding="utf-8") as f:
        lines = f.readlines()

    # Skip if the import already exists
    if any(line.strip() == "import java.util.*;" for line in lines):
        return

    insert_index = 0

    # If there's a package declaration, insert after it
    for i, line in enumerate(lines):
        stripped = line.strip()
        if stripped.startswith("package ") and stripped.endswith(";"):
            insert_index = i + 1
            break

    # Insert import (with a newline if needed for cleanliness)
    if insert_index < len(lines) and lines[insert_index].strip() != "":
        lines.insert(insert_index, "\n")
        insert_index += 1

    lines.insert(insert_index, IMPORT_LINE)

    with open(path, "w", encoding="utf-8") as f:
        f.writelines(lines)

    print(f"Updated: {path}")

def process_directory(root_dir):
    for root, _, files in os.walk(root_dir):
        for file in files:
            if file.endswith(".java"):
                process_java_file(os.path.join(root, file))

if __name__ == "__main__":
    # Change this to your target subdirectory
    target_directory = os.getcwd()
    process_directory(target_directory)
