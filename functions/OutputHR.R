OutputHR <- function(
                     alevel = 0 ,
                     ptrans = "z" ,
                     mtrans = "drop" ) {

    ### Get the coefficients from the coxph() model
    ###   according to the model
    C <- coef(
        summary(
            coxph(
                formula = BuildCoxFormula(
                    alevel = alevel ,
                    ptrans = ptrans ,
                    mtrans = mtrans ) ,
                data = D )))

    
    ### If there are adjustment variables exclude them from
    ###    the estimate table
    if( alevel > 0 ) {

        ### remember the rownames() since they are lost when
        ###   only one predictor row is selected
        RowNames <- rownames(C)

        ### Exclude unrelevant rows of the estimates table
        ###   selected by a regular expression match on
        ###   the variable name
        IndexOut <- grep( "A" , attr( C , "dimnames")[[1]] )
        
        ### Pick the relevant rows
        C <- C[ -IndexOut , ]
        
        ### if there is only one row
        ###   make it a 1x5-matrix and
        ###   reattach the predictor label
        if (is.null(dim(C))) {
            dim(C) <- c(1,5)
            rownames(C) <- RowNames[-IndexOut]}
        
    }
    
    HR <- sprintf( "%.2f" , round( exp( C[,1] ) , digits = 2 ))
    
    LCL <- sprintf( "%.2f" , round( exp(C[,1] - 1.96 * C[,3]) , digits = 2 ))
    UCL <- sprintf( "%.2f" , round( exp(C[,1] + 1.96 * C[,3]) , digits = 2 ))
    CI <- paste( "(" , LCL , "," , UCL , ")" , sep = "")
    
    P  <- sprintf( "%.3f" , round( C[,5] , digits = 3 ))
    P <- gsub( "0.000" , "<0.001" , P )
        
    OUT <- cbind( HR , CI , P )
    attr( OUT , "dimnames" )[[1]] <- attr( C , "dimnames" ) [[1]]
        
    return(OUT)
    
}    
