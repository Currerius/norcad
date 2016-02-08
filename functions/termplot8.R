termplot8 <- function( model , xlab = NULL , main = NULL ) {

    S <- termplot(
        model = model ,
        plot = FALSE ,
        se = TRUE )[[1]]

    S$lcl <- S$y - 1.96 * S$se
    S$ucl <- S$y + 1.96 * S$se

    ShadeX <- c( S$x , S$x[length(S$x):1] )
    ShadeY <- c( S$lcl , S$ucl[length(S$x):1] )

    AtAxes <- c(0.125,0.25,0.5,1,2,4,8)
    AtAxes <- AtAxes[
        which(
            (AtAxes >= 1/round(exp(-min(S$lcl)))) &
            (AtAxes <= round(exp(max(S$ucl)))))]
    
    plot(
        0 ,
        type = "n" ,
        bty = "n" ,
        axes = FALSE ,
        main = main ,
        xlab = xlab ,
        ylab = "Partial hazard" ,
        xlim = range( S$x ) ,
        ylim = range(log(AtAxes)) )

    axis(1)

    axis(2 , at = log(AtAxes) , label = AtAxes )

    polygon(
        x = ShadeX ,
        y = ShadeY ,
        border = FALSE ,
        col = "84228422" )

    abline( h = 0 , lty = 3 , lwd = 3 , col = "#FFFFFF" )

    lines(
        formula = y ~ x ,
        data = S ,
        lwd = 3 ,
        col = "#FFFFFF" )

    lines(
        formula = y ~ x ,
        data = S ,
        lwd = 2 ,
        col = "#84228484" )
}
