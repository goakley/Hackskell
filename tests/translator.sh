#!/bin/bash

VMDIR="../programs/vm/"
TRANSLATOR="../dist/build/HTranslate/HTranslate"
ASSEMBLER="../dist/build/HAssemble/HAssemble"
EMULATOR="../src/tools/HEmulate"
TMPFILE="/tmp/hack_translated_cmp"

FAILCOUNT=0
for dir in "$VMDIR"*; do
    "$TRANSLATOR" "$dir"/*.vm | "$ASSEMBLER" | "$EMULATOR" 16384 > "$TMPFILE"
    diff "$TMPFILE" "$dir"/*.cmp
    if [ $? -ne 0 ]; then
        FAILCOUNT=`expr $FAILCOUNT + 1`
        echo "FAILURE: $dir"
    else
        echo "SUCCESS: $dir"
    fi
done
