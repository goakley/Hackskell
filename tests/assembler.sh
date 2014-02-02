#!/bin/bash

PROGDIR="../programs/"
ASMDIR=$PROGDIR"asm/"
HACKDIR=$PROGDIR"hack/"
ASSEMBLER="../dist/build/HAssemble/HAssemble"
TMPFILE="/tmp/hack_assembled"

# assemble each .asm program and check for a matching .hack program
# a missing match means the .asm was improperly compiled

FAILCOUNT=0
for f in "$ASMDIR"*; do
    # send the assembly to a temp file
    cat "$f" | "$ASSEMBLER" > "$TMPFILE"
    FAILED=1
    # compare the temp file with each .hack file until matched
    for g in "$HACKDIR"*; do
        diff "$TMPFILE" "$g" > /dev/null
        if [ $? -eq 0 ]; then
            FAILED=0
            break
        fi
    done
    # check if a match was found or not
    if [ $FAILED -ne 0 ]; then
        FAILCOUNT=`expr $FAILCOUNT + 1`
        echo "FAILURE: $f"
    else
        echo "SUCCESS: $f"
    fi
done
# the return code is the number of failed assemblies
exit $FAILCOUNT
