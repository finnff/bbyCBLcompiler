#!/usr/bin/env bash

set -e

if [[ $# -ne 1 ]]; then
    echo "Usage: llpeek <file.ll>"
    exit 1
fi

INPUT="$1"
BASENAME="${INPUT%.ll}"
ASM_FILE="${BASENAME}.s"

llc -o "$ASM_FILE" "$INPUT"
cat "$ASM_FILE"
rm "$ASM_FILE"
