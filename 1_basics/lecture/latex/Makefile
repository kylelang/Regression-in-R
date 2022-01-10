# -*- Makefile -*-

### Assumptions:
### 1. You want to compile every RNW file in the current directory
### 2. After compilation, you want to move every PDF file to the "../slides/"
###    directory

## Find all RNW files beginning:
RNWS := $(wildcard *.Rnw)

## Strip the file extension off the located RNW files:
NAMES := $(RNWS:%.Rnw=%)

all: ${NAMES}

%: %.Rnw
	Rscript -e "library(knitr); knit(input = '$?', output = '$@.tex')"
	latexmk -pdf $@.tex

clean:
	mv *.pdf ../slides/
	latexmk -c
	rm -f *.tex *.nav *.snm *.vrb
