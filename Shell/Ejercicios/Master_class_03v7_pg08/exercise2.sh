#! /bin/bash

# Ejercicio 2.
# Create a dummy file with this command : seq 15> 20lines.txt; seq 9 1 20 >> 20lines.txt; echo"20\n20" >> 20lines.txt;
# (check the content of file first)

cd
seq 15 > 20lines.txt
seq 9 1 20 >> 20lines.txt
echo "20\n20" >> 20lines.txt

# a) Sort the lines of file based on alphanumeric characters

echo "ejercicio a"
sort -n 20lines.txt

# b) Sort the lines of file based on numeric values and eliminate the duplicates

echo "ejercicio b"
sort -n -u 20lines.txt

# c) Print just duplicated lines of the file

echo "ejercicio c"
sort -n 20lines.txt | uniq -d

# d) Print the line which has most repetitions

echo "ejercicio d"
sort -n 20lines.txt | uniq -c | sort -n -r | head -1

# e) Print unique lines with the number of repetitions sorted by the number of repetitions from lowest to highest

echo "ejercicio e"
sort -n 20lines.txt | uniq -c | sort -n


