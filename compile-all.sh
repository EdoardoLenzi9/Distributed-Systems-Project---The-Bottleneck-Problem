#!/bin/bash
input="directories.txt"
find "$(pwd)" -type d > "$input"
original_dir="$(pwd)"

while IFS= read -r var
do
    cd "$var"
    erl -make
done < "$input"

cd "$original_dir"
rm "$input"