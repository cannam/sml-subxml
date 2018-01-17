#!/bin/bash

set -eu

pass=0
fail=0

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

    if ./subxmlparse "$file" > out.xml ; then
        if xmllint out.xml >/dev/null ; then
            echo "--- pass: $file"
            pass=$(($pass + 1))
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
