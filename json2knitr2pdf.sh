#!/bin/bash

# Create the name of the knitr output document from the JSON file
NAME=`cat survival.json | jq -r '.Population,.Endpoint,.Predictor,.Modifier|.Label' | sed 's/ /_/g'`

# add the required .tex and replace whitespace with underscore
OUT=`echo \"$NAME.tex\" | sed 's/ /__/g'`

# the name of the knitr document
IN=\"Survival_Predictor_Modifier.Rnw\"

# start an R session and process the knitr document
echo 'knitr::knit2pdf( input = '$IN' , output = '$OUT')' | R --vanilla

# clean up the auxillary LaTeX files
rm -r figure *.aux *.log *.nav *.out *.snm *.tex *.toc *.vrb
