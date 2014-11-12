#!/usr/bin/env bash
Rscript -e "setwd('./'); getwd(); source('install_packages.R'); loadPackages('knitr'); source('CleanData.R'); knit('OurWritedown.Rnw');"
pdflatex OurWritedown.tex
bibtex OurWritedown.aux
pdflatex OurWritedown.tex
pdflatex OurWritedown.tex
open OurWritedown.pdf
