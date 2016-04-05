#!/bin/bash

while read insn ; do
    if ! [[ "$insn" =~ ^";;" ]]; then
        echo "------------------------------------"
        echo $insn
        mc=$(echo $insn | llvm-mc -arch=x86-64 -x86-asm-syntax=att --show-encoding --show-inst)
        echo "$mc"
        data=$(echo $mc | awk -vRS="]" -vFS="[" '{print $2}' | tr -d ',' | sed 's/0x/\\x/g')
        if [ -n "$data" ]; then
            echo "bap-mc lifter output:"
            bap-mc "$data" --show-bil
            echo "x86-lifter output:"
            echo $data | ./main.native
        fi
    fi
done < "test.s"
