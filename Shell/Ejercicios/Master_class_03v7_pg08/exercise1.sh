#! /bin/bash

#Ejercicio 1.
# Find top 10 files by size in your home directory including the subdirectories. Sort them by size and print the result including the size and the name of the file (hint: use find with –size and -exec ls –s parameters)

find ~/Data/ -exec ls -s {} + | sort -n -r | head -10

