#!/usr/bin/Rscript


 # load packages

require(quantreg)
require(Hmisc)
require(survival)
require(mgcv)
require(car)
require(smcfcs)
require(purrr)


 # load all R functions from the directory

lapply(
    X = dir( "./functions") ,
    FUN = function(X) source( paste( "./functions/" , X , sep = "" )))


 # open the JSON with the study design

JSON <- jsonlite::fromJSON( txt = "survival.json" , simplifyVector = FALSE)

Events <- sapply( JSON$Endpoints , with , Event )
Es <- paste( "E" , 0:(length(Events) - 1) , sep = "" )

Times <- sapply( JSON$Endpoints , with , Time )
Ts <- paste( "T" , 0:(length(Times) - 1) , sep = "" )

Predictors <- sapply( JSON$Predictors , with , Name )
Ps <- paste( "P" , 0:(length(Predictors) - 1) , sep = "" )

Modifiers <- sapply( JSON$Modifiers , with , Name )
Ms <- paste( "M" , 0:(length(Modifiers) - 1) , sep = "" )

Adjustments <- sapply( JSON$Adjustments , with , Name )
As <- paste( "A" , 0:(length(Adjustments) - 1) , sep = "" )
nLevels <- sapply( JSON$Adjustments , with , Level )

PatientCharacteristics <- sapply( JSON$PatientCharacteristics , with , Name )
PCs <- paste( "PC" , 0:(length(PatientCharacteristics) - 1) , sep = "" )

Forestplot <- sapply( JSON$Forestplot , with , Name )
Fs <- paste( "F" , 0:(length(Forestplot) - 1) , sep = "" )



 # create DB statements

dbVars <- paste0(
    c(
        paste( Events , "AS" , Es ) ,
        paste( Times , "AS" , Ts ) ,
        paste( Predictors , "AS" , Ps ) ,
        paste( Modifiers , "AS" , Ms ) ,
        paste( Adjustments , "AS" , As) ,
        paste( PatientCharacteristics , "AS" , PCs ) ,
        paste( Forestplot , "AS" , Fs )) ,
        collapse = ", ")

dbWhere <- paste0(
    sapply(
        JSON$Population$Subset ,
        with ,
        Where ) ,
    collapse = " AND " )
        
dbStatement <- paste(
    "SELECT" ,
    dbVars ,
    "FROM PATIENT_INFO P
       LEFT JOIN LAB_BLOOD_TOTAL B ON P.FORUS_ID=B.FORUS_ID
       LEFT JOIN hs_medikament_skjema M ON P.FORUS_ID=M.FORUS_ID
       LEFT JOIN hs_hendelser_CVDNOR_mars14 C ON P.NORCAD_ID=C.NORCAD_ID
         WHERE" ,
    dbWhere )


 # connect to DB and get data

if ( !exists("con") )
    con <- DBI::dbConnect(
        drv = RMySQL::MySQL() ,
        dbname = JSON$Population$Database )

D <- DBI::dbGetQuery(
    conn = con , 
    statement = dbStatement)



 # create survival objects for Cox regression

for (i in 0:(length(Events)-1)) {
    D[ , paste("Surv" , i , sep = "") ] <- Surv(
        time = D[,paste("T" , i , sep = "") ] ,
        event = D[,paste("E" , i , sep = "") ] )
}


 # create transformations of the original variables

for (i in 0:(length(Predictors)-1)) {
    D[ , paste("zP" , i , sep = "") ] <- z( D[ , paste("P" , i , sep = "") ] )
    D[ , paste("zlogP" , i , sep = "") ] <- zlog( D[ , paste("P" , i , sep = "") ] )
    D[ , paste("qP" , i , sep = "") ] <- q( D[ , paste("P" , i , sep = "") ] )
    D[ , paste("c2P" , i , sep = "") ] <- cutN( D[ , paste("P" , i , sep = "") ] , 2)
    D[ , paste("n2P" , i , sep = "") ] <- as.numeric( cutN( D[ , paste("P" , i , sep = "") ] , 2))
    D[ , paste("c3P" , i , sep = "") ] <- cutN( D[ , paste("P" , i , sep = "") ] , 3)
    D[ , paste("n3P" , i , sep = "") ] <- as.numeric( cutN( D[ , paste("P" , i , sep = "") ] , 3))
    D[ , paste("c4P" , i , sep = "") ] <- cutN( D[ , paste("P" , i , sep = "") ] , 4)
    D[ , paste("n4P" , i , sep = "") ] <- as.numeric( cutN( D[ , paste("P" , i , sep = "") ] , 4))
    D[ , paste("c5P" , i , sep = "") ] <- cutN( D[ , paste("P" , i , sep = "") ] , 5)
    D[ , paste("n5P" , i , sep = "") ] <- as.numeric( cutN( D[ , paste("P" , i , sep = "") ] , 5))
}


for (i in 0:(length(Modifiers)-1)) {
    D[ , paste("zM" , i , sep = "") ] <- z( D[ , paste("M" , i , sep = "") ] )
    D[ , paste("zlogM" , i , sep = "") ] <- zlog( D[ , paste("M" , i , sep = "") ])
    D[ , paste("qM" , i , sep = "") ] <- q( D[ , paste("M" , i , sep = "") ] )
    D[ , paste("c2M" , i , sep = "") ] <- cutN( D[ , paste("M" , i , sep = "") ] , 2)
    D[ , paste("n2M" , i , sep = "") ] <- as.numeric( cutN( D[ , paste("M" , i , sep = "") ] , 2))
    D[ , paste("c3M" , i , sep = "") ] <- cutN( D[ , paste("M" , i , sep = "") ] , 3)
    D[ , paste("n3M" , i , sep = "") ] <- as.numeric( cutN( D[ , paste("M" , i , sep = "") ] , 3))
    D[ , paste("c4M" , i , sep = "") ] <- cutN( D[ , paste("M" , i , sep = "") ] , 4)
    D[ , paste("n4M" , i , sep = "") ] <- as.numeric( cutN( D[ , paste("M" , i , sep = "") ] , 4))
    D[ , paste("c5M" , i , sep = "") ] <- cutN( D[ , paste("M" , i , sep = "") ] , 5)
    D[ , paste("n5M" , i , sep = "") ] <- as.numeric( cutN( D[ , paste("M" , i , sep = "") ] , 5))
}




   ### prepare the export directory and
   ### switch to it for reporting

system2(
    command = "rm" ,
    args = c("-r","export"))

system2(
    command = "mkdir" ,
    args = c("export"))

system2(
    command = "cp" ,
    args = c("*.Rmd","./export/"))

system2(
    command = "cp" ,
    args = c("*.json","./export/"))

setwd( "./export/" )


   # export the dataset
save( D , file = paste( JSON$Population$Database , "Rdata" , sep = "." ))



   ### prepare knitr

require(knitr)
opts_chunk$set(warnings=FALSE,echo=FALSE)
options(width="130")


   ### Rscript iterates through all enpoint, predictor, modifier combinations and
   ### compiles the knitr files to tex

for (n.P in 1:length(Ps)) {

    PCFileName <- gsub(
        " " , "" ,
        paste(
            "PatientCharacteristics",
            sapply( JSON$Predictors , with , ShortLabel )[n.P] ,
            sep = "-" ))


    PCOutfile <- paste( PCFileName , "md" , sep = "." )

    knit2html(
        input = "Patient_Characteristics.Rmd" ,
        stylesheet = "/home/rforge/Documents/github/tufte-css/tufte.css",
        output = PCOutfile )
    
    for (n.M in 1:length(Ms)) {
        for (n.Surv in 1:length(Es)) {

            # Build the name of the PDF outfile from the
            # endpoint, predictor, modifier combination:
            SurvFileName <- gsub(
                " " , "" ,
                paste(
                    "SurvivalAnalysis" ,
                    sapply( JSON$Endpoints , with , ShortLabel )[n.Surv] ,
                    sapply( JSON$Predictors , with , ShortLabel )[n.P] ,
                    sapply( JSON$Modifiers , with , ShortLabel )[n.M] ,
                    sep = "-" ))

            SurvivalOutfile <- paste(
                SurvFileName ,
                ".tex" ,
                sep = "" )

            opts_chunk$set(fig.path=paste("./figure/" , SurvFileName , "-" , sep = "" ))

            knit2html(
                input = "Survival_Predictor_Modifier.Rmd",
                stylesheet = "/home/rforge/Documents/github/tufte-css/tufte.css",
                output = SurvivalOutfile )
            
        }
    }
}


# pandoc -c /home/rforge/Documents/github/tufte-css/tufte.css -s -S -i -t html --mathjax Patient_Characteristics.md -o PCtmp.html
# pandoc -c https://raw.githubusercontent.com/edwardtufte/tufte-css/master/tufte.css -s -S -i -t html --mathjax Patient_Characteristics.md -o PCtmp.html
# pandoc  -s -S -i -t dzslides --mathjax Check_SDMA.md -o Check_SDMA_dzslides.html




# clean up the auxillary markdown

system2(
    command = "rm" ,
    args = c("*.Rmd","*.md"),
    wait = FALSE)


# zip the resulting pdf's, dataset, exported figures and tables
    
system2(
    command = "zip" ,
    args = c("export.zip","*.html","./figure/","*.json","*.Rdata"))

# email to Rforge
system2(
    command = "swaks" ,
    args = c(
        "--server","localhost" ,
        "--port","8025" ,
        "--from","Rscript@currerius.com" ,
        "--to","seifert.reinhard@gmail.com" ,
        "--attach" , "export.zip" ,
        "--suppress-data" ,
        "--h-Subject" , "Analyses_from_NORCAD" ,
        "--body","survival.json"))
