MeansOverGroups <- function( measure , groups ){

    MarginalMean <- coef(
        summary(
            lm(
                formula = as.formula(
                    paste( measure , "~" , groups , "- 1" )) ,
                data = D )))[,1:2]

    PforTrend <- sprintf(
        fmt = "%.3f" ,
        coef(
            summary(
                lm(
                    formula = as.formula(
                        paste( measure , "~ as.numeric(", groups , ")" )) ,
                    data = D )))[8])
    if (PforTrend == "0.000") PforTrend <- "<0.001"

    LINE <- c(
        rbind(
            signif(
                x = MarginalMean[,1] ,
                digits = 3) ,
            signif(
                x = MarginalMean[,2] ,
                digits = 2) ) ,
        PforTrend)

    attr( LINE , "names" ) <- c(
        rep(
            x = c("Mean","SE") ,
            times = ( length( LINE ) - 1 )/2 ) ,
        "P" )

    return(LINE) }
