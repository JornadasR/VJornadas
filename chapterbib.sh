#!/bin/bash
cd resumenes
for file in *.aux ; do
bibtex `basename $file .aux`
done
cd ..

# http://stackoverflow.com/questions/2765209/latex-bibliography-per-chapter
