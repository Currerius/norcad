
z <- function(X) (X - mean( X , na.rm = TRUE )) / sd( X , na.rm = TRUE )


zlog <- function(X) {

    # to avoid zeros in X before logging set X to
    # "something very low" , but not absurdly
    # such not to produce  outliers
    X[X==0] <- min( X[X!=0] , na.rm = TRUE ) / 2
        
    return( (log(X) - mean( log(X) , na.rm = TRUE )) / sd( log(X) , na.rm = TRUE ))}


q <- function(X) 100 * rank( X , na.last = "keep" ) / length( na.exclude(X) )


cutN <- function(X,n=4) cut(
                          x = X + rnorm(
                                      n = length(X) ,
                                      mean = 0 ,
                                      sd = 10^-10 ) ,
                          include.lowest = TRUE ,
                          breaks = quantile(
                              x = X ,
                              na.rm = TRUE ,
                              probs = 0:n/n ))
