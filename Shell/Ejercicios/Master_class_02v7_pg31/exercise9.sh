#! /bin/bash

# Ejercicio 9.
# Enunciado. Move “first_dir /sub3/text_file.txt.backup” to “first_dir” directory as hidden file.

cd ~/first_dir/sub3
mv text_file.txt.backup ~/first_dir/.text_file.txt.backup
