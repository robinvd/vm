#!/bin/sh
for i in `ls $(dirname "$0")/tests/*.vmb`; do
    echo $i
    cargo run -- $i
done