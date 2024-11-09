#!/bin/bash

(( i = 1 ))

files=(comments conditions escapes functional functions gc hello hello1 hello2 hello3 hello5 integers lib merge nested pure pureClosure pureTree record spill strings vars)

while true; do
    echo "******************************"
    echo Capacity $i
    echo "******************************"

    export TIGER_GC_CAPACITY=$i

    for file in ${files[*]}; do
        if [ -f "tests/$file.stdin" ]; then
            output=$("tests/$file" < "tests/$file.stdin" && printf X)
        else
            output=$("tests/$file" && printf X)
        fi
        # Add an X and then remove it so that it doesn't strip trailing newlines.
        output=${output%X}
        if ! diff <(printf "%s" "$output") "tests/$file.stdout"; then
            diff -y <(printf "%s" "$output") "tests/$file.stdout"
            echo "$file FAIL"
            exit
        fi
    done

    (( i = i + 1 ))
done
