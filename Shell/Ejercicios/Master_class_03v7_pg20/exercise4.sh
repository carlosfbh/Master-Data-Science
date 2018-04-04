#! /bin/bash

#4. 
#Find all files inside Data with txt extension that have word “Science” (case unsensitive) inside the content. Print file path and the line contaning the (S/s)cience word.

cd
grep -r -n -i "Science" /home/dsc/Data *.txt
