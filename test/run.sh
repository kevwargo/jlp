#!/bin/bash

cd $(dirname $(dirname $0))

VERSION=$(mvn help:evaluate -Dexpression=project.version -q -DforceStdout)

exitcode=0
for t in $(ls test | grep -oP '.+(?=\.lisp$)'); do
    if diff -u test/${t}.log <(java -jar target/jlp-${VERSION}-jar-with-dependencies.jar test/${t}.lisp | sed -e '$a\'); then
        echo "$t OK"
    else
        exitcode=1
    fi
done

exit $exitcode
