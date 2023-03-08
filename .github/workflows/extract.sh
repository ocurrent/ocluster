#!/bin/bash

INPUT=("install/bin")
OUTPUT=ocluster-windows

mkdir -p "$OUTPUT"
for dir in "${INPUT[@]}"; do
    for exe in "$dir"/*.exe ; do
        for dll in $(PATH="/usr/x86_64-w64-mingw32/sys-root/mingw/bin:$PATH" cygcheck "$exe" | grep -F x86_64-w64-mingw32 | sed -e 's/^ *//'); do
            if [ ! -e "$OUTPUT/$(basename "$dll")" ] ; then
                cp "$dll" "$OUTPUT/"
            else
                printf "%s uses %s (already extracted)\n" "$exe" "$dll"
            fi
        done
        printf "Extracted %s\n" "$exe"
        cp "$exe" "$OUTPUT/"
    done
done
rm -f "$OUTPUT/dummy.exe"
