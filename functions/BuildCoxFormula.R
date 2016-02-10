BuildCoxFormula <- function(
                         alevel = 0 ,
                         ptrans = "z" ,
                         psmooth = FALSE ,
                         mtrans = "drop"
                         ) {

    
    ### Choose the correct predictor variable according to
    ###    the chosen transformation "ptrans"
    ###    the transformations are created automatically
    P <- if (
        ptrans %in% c("","q","z","zlog","n2","c2","n3","c3","n4","c4","n5","c5")
    ) {
             paste( ptrans , "P" , n.P-1 , sep = "" )
         } else warning("Invalid value of ptrans.\nPossible values: q, z, zlog, n2, c2, n3, c3, n4, c4, n5, c5 or empty string.")

    if (psmooth) P <- paste("pspline(",P,")")
    
    ### Choose the correct modifier variable according to
    ###    the chosen transformation "mtrans"
    M <- if (
        mtrans %in% c("","q","z","zlog","n2","c2","n3","c3","n4","c4","n5","c5","drop")
    ) {
             if ( mtrans == "drop" ) NULL else paste( "*", mtrans , "M" , n.M-1 , sep = "" )
         } else warning("Invalid value of ptrans.\nPossible values: drop, q, z, zlog, n2, c2, n3, c3, n4, c4, n5, c5 or empty string.")
    

    
    ### Concatenate all adjustment variables according to
    ###    the chosen adjustment level "alevel" with a leading "+"
    A <- paste0(
        c(
            "" ,
            paste(
                "A" ,
                0:(length(Adjustments)-1) ,
                sep = "")[ which( nLevels <= alevel )]
           ) ,
            collapse = " + " )

    S <- paste( "Surv" , n.Surv-1 , " ~" , sep = "" )
    
    ### Return the model formula for the coxph() model
    return(as.formula( paste( S , P , M , A )))
    
}
