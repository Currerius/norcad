PrintCoxSubgroupTable <- function(
                     alevel = 0 ,
                     ptrans = "z" ,
                     n.P = 1 , n.M = 1 , n.Surv = 1
                     ) {

    SUB <- D[ , paste( "n2M" , n.M-1 , sep = "" )]
    i1 <- which(SUB==1)
    i2 <- which(SUB==2)

    Clow <- coef(
        summary(
            coxph(
                formula = BuildCoxFormula(
                    alevel = alevel ,
                    ptrans = ptrans ,
                    mtrans = "drop" ,
                    n.P = n.P , n.M = n.M , n.Surv = n.Surv
                ) ,
                data = D[i1,] )))

    Chigh <- coef(
        summary(
            coxph(
                formula = BuildCoxFormula(
                    alevel = alevel ,
                    ptrans = ptrans ,
                    mtrans = "drop" ,
                    n.P = n.P , n.M = n.M , n.Surv = n.Surv
                ) ,
                data = D[i2,] )))
    
    
    ### If there are adjustment variables exclude them from
    ###    the estimate table
    if( alevel > 0 ) {

        ### remember the rownames() since they are lost when
        ###   only one predictor row is selected
        RowNames <- rownames(Clow)

        ### Exclude unrelevant rows of the estimates table
        ###   selected by a regular expression match on
        ###   the variable name
        IndexOut <- grep( "A" , rownames(Clow) )

        ### Pick the relevant rows
        Clow <- Clow[ -IndexOut , ]
        Chigh <- Chigh[ -IndexOut , ]
        
        ### if there is only one row
        ###   make it a 1x5-matrix and
        ###   reattach the predictor label
        if (is.null(dim(Clow))) {
            dim(Clow) <- c(1,5)
            rownames(Clow) <- RowNames[-IndexOut]
            dim(Chigh) <- c(1,5)
            rownames(Chigh) <- RowNames[-IndexOut]}
    }
    
    HRlow <- sprintf( "%.2f" , round( exp( Clow[,1] ) , digits = 2 ))
    
    LCLlow <- sprintf( "%.2f" , round( exp(Clow[,1] - 1.96 * Clow[,3]) , digits = 2 ))
    UCLlow <- sprintf( "%.2f" , round( exp(Clow[,1] + 1.96 * Clow[,3]) , digits = 2 ))
    CIlow <- paste( "(" , LCLlow , "," , UCLlow , ")" , sep = "")
    
    HRhigh <- sprintf( "%.2f" , round( exp( Chigh[,1] ) , digits = 2 ))
    
    LCLhigh <- sprintf( "%.2f" , round( exp(Chigh[,1] - 1.96 * Chigh[,3]) , digits = 2 ))
    UCLhigh <- sprintf( "%.2f" , round( exp(Chigh[,1] + 1.96 * Chigh[,3]) , digits = 2 ))
    CIhigh <- paste( "(" , LCLhigh , "," , UCLhigh , ")" , sep = "")
    
    ### Get the coefficients from the coxph() interaction model
    ### with a dichotomous modifier
    Ci <- coef(
        summary(
            coxph(
                formula = BuildCoxFormula(
                    alevel = alevel ,
                    ptrans = ptrans ,
                    mtrans = "c2" ) ,
                data = D )))

    ### the P value of the interaction term is always the last parameter
    P  <- sprintf( "%.3f" , round( Ci[length(Ci)] , digits = 3 ))
    P <- gsub( "0.000" , "<0.001" , P )
        
    OUT <- cbind( HRlow , CIlow , HRhigh , CIhigh , P )
    attr( OUT , "dimnames" )[[1]] <- attr( Clow , "dimnames" ) [[1]]
    attr( OUT , "dimnames" )[[2]] <- c( "HR","CI","HR","CI","P-value")
        
    return(OUT)
    
}    
