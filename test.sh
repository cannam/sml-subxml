#!/bin/bash

set -eu

pass=0
fail=0

if [ ! -d testfiles/good ]; then
    echo "Run this from the repo root"
    exit 2
fi

mkdir -p testfiles/tmp
outdir=testfiles/tmp

for file in testfiles/bad/*.xml ; do

    if ./subxmlparse "$file" ; then
        echo "*** FAIL: $file should have been rejected"
        fail=$(($fail + 1))
    else
        echo "--- pass: $file correctly rejected"
        pass=$(($pass + 1))
    fi

done

for file in testfiles/good/*.xml ; do

    outfile="$outdir/out.xml"
    lintfile_orig="$outdir/lint_orig.xml"
    lintfile_mine="$outdir/lint_mine.xml"
    
    if ./subxmlparse "$file" > "$outfile" ; then
        if xmllint --encode UTF-8 --format --output "$lintfile_mine" "$outfile" >/dev/null ; then
            xmllint --encode UTF-8 --format "$file" |
                # Our expected diversion from the original
                grep -v '<!-- Comment before document, ignored -->' |
                grep -v '<!-- Comment after document, ignored -->' > "$lintfile_orig"
            if ! diff -q "$lintfile_orig" "$lintfile_mine" >/dev/null; then
                echo "*** FAIL: $file output does not match expected"
                echo; echo "*** Input:"
                cat "$file"
                echo "*** Output:"
                cat "$outfile"
                echo "*** Linted input:"
                cat "$lintfile_orig"
                echo "*** Linted output (should match linted input):"
                cat "$lintfile_mine"
                echo "***"; echo
                fail=$(($fail + 1))
            else
                echo "--- pass: $file"
                pass=$(($pass + 1))
            fi
        else
            echo "*** FAIL: $file"
            fail=$(($fail + 1))
        fi
    else 
        echo "*** FAIL: $file"
        fail=$(($fail + 1))
    fi

done

echo
echo "Passed: $pass"
echo "Failed: $fail"
