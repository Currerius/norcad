DescribeFac <- function(X) {

    OUT <- addmargins( table( D[ , X ] , useNA="a" ))[c(4,3,1,2)]
    names(OUT)[1:2] <- c("n","missing")
    
    return(OUT)}
