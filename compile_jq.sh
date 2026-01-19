#!/usr/bin/env bash
mkdir -p build
non_import=""
for file in $@; do
    if [[ "$file" == *import.scm ]]; then
        cp "$file" build/
    else
        non_import="$non_import $file"
        cat "$file" | sed -e 's/; *(declare/(declare/' > build/$file
    fi
done
cd build
csc -static $non_import && cp jq ../
