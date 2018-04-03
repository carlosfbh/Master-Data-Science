#! /bin/bash

# Ejercicio 3.
# Create another file with this command : seq 0 2 40 > 20lines2.txt

cd
seq 0 2 40 > 20lines2.txt

# a) Print combining the first two files (20lines.txt and 20lines2.txt) but without duplicates

echo "Ejercicio a"
cat 20lines.txt 20lines2.txt | sort -n | uniq


# b) Merge the first two files. Print unique lines together with the number of occurrences inside the merged file and sorted based on line content.

echo "Ejercicio b"
cat 20lines.txt 20lines2.txt | sort -n | uniq -c
