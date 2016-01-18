BuildCoxFormula <- function(
                         alevel = 0 ,
                         ptrans = "z" ,
                         mtrans = "drop" ) {

    ### Choose the correct predictor variable according to
    ###    the chosen transformation "ptrans"
    ###    the transformations are created automatically
    P <- if (
        ptrans %in% c("","d","q","z","zlog","trend4","ntil4")
    ) {
             paste( ptrans , Predictor , sep = "" )
         } else warning("Invalid value of ptrans. Possible values: d,q,z,zlog or empty string.")

    
    ### Choose the correct modifier variable according to
    ###    the chosen transformation "mtrans"
    M <- switch(
        mtrans ,
        "drop" = NULL ,
        "none" = paste( "*" , Modifier , sep = "" ) ,
        "z" = paste( "*z" , Modifier , sep = "" ) ,
        "zlog" = paste( "*zlog" , Modifier , sep = "" ) ,
        "q" = paste( "*q" , Modifier , sep = "" ) ,
        "d" = paste( "*d" , Modifier , sep = "" ) ,
        warning(
            "Invalid value of ptrans.\nPossible values: d, q, z, zlog, none, drop.")
    )
    
    ### Concatenate all adjustment variables according to
    ###    the chosen adjustment level "alevel" with a leading "+"
    A <- paste0( c( "" , Adjustments[ which( nLevels <= alevel ) ] ) , collapse = " + " )
    
    ### Return the model formula for the coxph() model
    return(as.formula( paste( "Survival ~" , P , M , A )))
    
}
