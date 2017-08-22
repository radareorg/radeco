#!/bin/bash

# Script used to trigger a potential bug
# Usage: ./bug-digger.sh "YOURCOMMAND"
# Example: ./bug-digger.sh "cargo test analysis::valueset::fixcall::test::fix_test -- --nocapture"

num=0
echo "Begin"
while [ $? -eq 0 ]
do
        echo $num
        num=`expr $num + 1`
        $1 > /tmp/out
done

# Output err
cat /tmp/out
