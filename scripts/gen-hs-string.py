#!/usr/bin/env python3

import sys


def main():
    input_text = sys.stdin.read()
    lines = input_text.splitlines()

    if not lines:
        print('""')
        return

    for i, line in enumerate(lines):
        # Escape backslashes and quotes
        escaped_line = line.replace('\\', '\\\\').replace('"', '\\"')

        if i == 0:
            print('"' + escaped_line + '\\n\\')
        elif i < len(lines) - 1:
            print('\\' + escaped_line + '\\n\\')
        else:
            print('\\' + escaped_line + '"')


if __name__ == "__main__":
    main()
