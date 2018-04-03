#! /bin/bash

# Ejercicio 2.
# Enunciado. Find all empty files inside subdirectories of your home directory which do NOT have read-write-execute permissions given to all users

find ~/ -type f -empty -! -perm 777 -exec ls -l {} \;
