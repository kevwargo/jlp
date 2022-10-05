#!/bin/bash

CLASSPATH=$(realpath $(dirname $0)/../target/classes)

cd $(dirname $0)

exitcode=0
for t in $(ls | grep -oP '.+(?=\.lisp$)'); do
    if diff -u ${t}.log <(java -cp $CLASSPATH kevwargo.jlp.Main ${t}.lisp | sed -e '$a\'); then
        echo "$t OK"
    else
        exitcode=1
    fi
done

exit $exitcode
