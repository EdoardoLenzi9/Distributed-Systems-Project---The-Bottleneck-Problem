#!/bin/bash

rm *.out
rm *.aux
rm *.log
rm *.bbl
rm *.blg
rm *.fls
rm *.fdb_latexmk

pdflatex report.tex
pdflatex report.tex
pdflatex report.tex

bibtex report

pdflatex report.tex
pdflatex report.tex