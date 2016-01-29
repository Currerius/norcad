MediansOverGroups <- function( measure , groups ){
    MarginalMedian <- coef(
        summary(
            rq(
                formula = as.formula(
                    paste( measure , "~" , groups , "- 1" )) ,
                tau = 0.5 ,
                data = D )))[,1:2]
    PforTrend <- sprintf(
        fmt = "%.3f" ,
        coef(
            summary(
                rq(
                    formula = as.formula(
                        paste( measure , "~ as.numeric(" , groups , ")" )) ,
                    tau = 0.5 ,
                    data = D )))[8])
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
    "Median","SE","Median","SE","Median","SE","Median","SE","P")
    return(LINE) }
