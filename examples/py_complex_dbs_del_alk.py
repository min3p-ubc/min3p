"""
Recursively find all "complex.dbs" files under ROOT_DIR and fix their
odd-numbered lines (line 1, 3, 5, ... using 1-based line numbering).
Even-numbered lines are left completely untouched.

Expected format of each odd line:
    <name>  <num1> <num2> <num3> <num4> <num5> <num6> [<num7>] [<num8>]

Numbers may be separated by single or multiple spaces, and may be in
plain float format (1.23, -0.5) or scientific/Fortran-D format
(1.23E+05, 1.23e-05, 1.23D-05).

Rule:
    - 6 numbers  -> leave the line unchanged
    - 7 numbers  -> delete the 7th number
    - 8 numbers  -> delete the 7th number (leaving 7 numbers)
    - anything else (not 6/7/8 numbers, or tokens that don't look like
      numbers) -> line is left unchanged and reported as a warning

A .bak backup of each modified file is kept next to the original
(e.g. complex.dbs.bak) unless MAKE_BACKUP is set to False.
"""

import os
import re
import shutil

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

ROOT_DIR = r"."          # folder to search recursively
TARGET_FILENAME = "complex.dbs"
MAKE_BACKUP = True

# Matches a real number in plain, scientific, or Fortran-D exponent format
NUMBER_RE = re.compile(
    r"^[+-]?(\d+\.\d*|\.\d+|\d+)([eEdD][+-]?\d+)?$"
)

# ---------------------------------------------------------------------------
# Core logic
# ---------------------------------------------------------------------------


def fix_line(line, line_no, file_path):
    """Return the (possibly) modified line, and whether it was changed."""
    tokens = list(re.finditer(r"\S+", line))

    if len(tokens) < 2:
        return line, False  # nothing to do (blank/short line)

    name_tok = tokens[0]

    # Only take the run of CONSECUTIVE numeric tokens right after the name.
    # Anything after that (e.g. a trailing "!comment") is left alone.
    number_toks = []
    for t in tokens[1:]:
        if NUMBER_RE.match(t.group()):
            number_toks.append(t)
        else:
            break

    n = len(number_toks)

    if n == 0:
        print(f"  [WARN] {file_path}: line {line_no} - no numeric tokens "
              f"found after name '{name_tok.group()}', left unchanged")
        return line, False

    if n == 6:
        return line, False  # already correct, nothing to do

    if n not in (7, 8):
        print(f"  [WARN] {file_path}: line {line_no} - expected 6-8 numbers, "
              f"found {n}, left unchanged")
        return line, False

    # Remove the 7th number (index 6 in number_toks), together with the
    # whitespace that precedes it, so spacing around the remaining numbers
    # stays intact.
    seventh = number_toks[6]
    sixth = number_toks[5]

    new_line = line[:sixth.end()] + line[seventh.end():]
    return new_line, True


def process_file(file_path):
    with open(file_path, "r", newline="") as f:
        lines = f.readlines()

    changed = False
    new_lines = []

    for idx, line in enumerate(lines):
        line_no = idx + 1  # 1-based
        if line_no % 2 == 1:  # odd line
            new_line, was_changed = fix_line(line, line_no, file_path)
            if was_changed:
                changed = True
            new_lines.append(new_line)
        else:
            new_lines.append(line)

    if changed:
        if MAKE_BACKUP:
            shutil.copy2(file_path, str(file_path) + ".bak")
        with open(file_path, "w", newline="") as f:
            f.writelines(new_lines)
        print(f"[FIXED] {file_path}")

    return changed


def main():
    total_files = 0
    fixed_files = 0

    for root, _dirs, files in os.walk(ROOT_DIR):
        for fname in files:
            if fname == TARGET_FILENAME:
                total_files += 1
                fpath = os.path.join(root, fname)
                if process_file(fpath):
                    fixed_files += 1

    print("\n----------------------------------------")
    print(f"Found  : {total_files} '{TARGET_FILENAME}' file(s)")
    print(f"Fixed  : {fixed_files} file(s) had at least one line changed")
    print("----------------------------------------")


if __name__ == "__main__":
    main()