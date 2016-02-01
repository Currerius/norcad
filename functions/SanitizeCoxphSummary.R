SanitizeCoxphSummary <- function( model , rows = 1 , labels = "Predictor" ){
    Estimates <- summary( model ) $ conf.int [ rows , -2 ]
    P.values <- summary( model ) $ coefficients [ rows , 5 ]
    if ( length(rows) == 1 ) {
        Table <- round(
            t(
                c(
                    Estimates ,
                    P.values)) ,
            digits = 3)}
    else {
        Table <- round(
            cbind(
                Estimates ,
                P.values) ,
            digits = 3)}
    attr( Table , "dimnames" )[[1]] <- labels
    attr( Table , "dimnames" )[[2]] <- c("HR" , "LCL" , "UCL" , "P")
    return( Table ) }
