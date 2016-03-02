MediansOverGroups <- function( measure , groups ){
    
      # since ranks statistics are sensitive to ties
      # add a tiny bit of random noise
    thisDATA <- D[ , c(measure , groups ) ]
    thisDATA[,1] <- thisDATA[,1] + rnorm( n = length(thisDATA[,1]) , mean = 0 , sd = .000001 )
    names(thisDATA) <- c("measure","groups")

    MarginalMedian <- coef(
        summary(
            rq(
                formula = measure ~ groups -1 ,
                method = "fn" ,
                tau = 0.5 ,
                data = thisDATA )))[,1:2]
    
    PforTrend <- sprintf(
        fmt = "%.3f" ,
        coef(
            summary(
                rq(
                    formula = measure ~ as.numeric(groups) ,
                    method = "fn" ,
                    tau = 0.5 ,
                    data = thisDATA )))[8])
    
    if (PforTrend == "0.000") PforTrend <- "<0.001"
    
    LINE <- c(
        rbind(
            signif(
                x = MarginalMedian[,1] ,
                digits = 3) ,
            signif(
                x = MarginalMedian[,2] ,
                digits = 2) ) ,
        PforTrend)

    attr( LINE , "names" ) <- c(
        rep(
            x = c("Median","SE") ,
            times = ( length( LINE ) - 1 )/2 ) ,
        "P" )

    return(LINE) }
