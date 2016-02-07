#!/bin/bash

# Create the name of the knitr output document from the JSON file
NAME=`cat survival.json | jq -r '.Population,.Endpoints[0],.Predictors[0],.Modifiers[0]|.Label' | sed 's/ /_/g'`

# add the required .tex and replace whitespace with underscore
OUT=`echo \"$NAME.tex\" | sed 's/ /__/g'`

# the name of the knitr document
IN=\"Survival_Predictor_Modifier.Rnw\"

# start an R session and process the knitr document
echo '
n.P=1
n.M=1
n.Surv=1
knitr::knit2pdf( input = '$IN' , output = '$OUT')' | R --vanilla


# clean up the auxillary LaTeX files
rm -r figure *.aux *.log *.nav *.out *.snm *.tex *.toc *.vrb
