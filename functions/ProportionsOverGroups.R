ProportionsOverGroups <- function( factor , groups ){
    Count <- xtabs( 
        formula = as.formula(
            paste( "~" ,  groups , "+" , factor )) ,
        data = D )
    Proportion <- round(
        100 * prop.table(
            x = Count ,
            margin = 1 ) ,
        digits = 1 )
    PforTrend <- sprintf(
        fmt = "%.3f" ,
        coef(
            summary(
                glm(
                    formula = as.formula(
                        paste( factor , "~ as.numeric(", groups ,")" )) ,
                    family = "binomial" ,
                    data = D )))[8])
    if (PforTrend == "0.000") PforTrend <- "<0.001"
    LINE <- c(
        rbind(
            sprintf(
                fmt = "%.1f",
                Proportion[,2]) ,
            sprintf(
                fmt = "%.0f",
                Count[,2]) ) ,
        PforTrend)
    attr( LINE , "names" ) <- c(
        rep(
            x = c("%","N") ,
            times = length( Count[,2] ) ) ,
        "P" )
    return(LINE)}
