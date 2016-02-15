BuildIndependent <- function(
                         alevel = 0 ,
                         ptrans = "z" ,
                         mtrans = "drop"
                         ) {

    ### Choose the correct predictor variable according to
    ###    the chosen transformation "ptrans"
    ###    the transformations are created automatically
    P <- if (
        ptrans %in% c("","q","z","zlog","n","c")
    ) {
             paste( ptrans , "P" , n.P-1 , sep = "" )
         } else warning("Invalid value of ptrans.\nPossible values: q, z, zlog, n, c or empty string.")

    if (psmooth) P <- paste("pspline(",P,")")

    
    ### Choose the correct modifier variable according to
    ###    the chosen transformation "mtrans"
    M <- if (
        mtrans %in% c("","q","z","zlog","n","c","drop")
    ) {
             if ( mtrans == "drop" ) NULL else paste( "*", mtrans , "M" , n.M-1 , sep = "" )
         } else warning("Invalid value of ptrans.\nPossible values: drop, q, z, zlog, n, c or empty string.")
    

    
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

    
    ### Return the model formula for the coxph() model
    return(paste( P , M , A , sep = ""))
    
}
