#!/bin/bash

# Script to compile and run assembly code.
source_file=$1
asm_file=$(echo ${source_file} | sed -e "s/\.pr$/\.s/")
obj_file=${source_file%.*}

cargo run -- ${source_file} -o ${asm_file}
if [ $? -eq 0 ]; then
    gcc -g -o ${obj_file} ${asm_file}
    ./${obj_file}
    echo $?
    rm ${asm_file}
    rm ${obj_file}
fi
