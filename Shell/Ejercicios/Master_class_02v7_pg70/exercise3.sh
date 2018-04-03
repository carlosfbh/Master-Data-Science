#! /bin/bash

# Ejercicio 3.
# Enunciado. Expand previous command to grant these permissions using “ok cmd” option.

find ~/ -type f -empty -! -perm 777 -exec ls -l {} \; -ok chmod 777 {} \;
