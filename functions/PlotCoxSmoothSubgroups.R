PlotCoxSmoothSubgroups <- function( ptrans = "q" , alevel = 1 , xlab = "") {
    
    F <- BuildCoxFormula( alevel = alevel , ptrans = ptrans , psmooth = TRUE , mtrans = "drop" )

    cox1 <- coxph(
        formula = F ,
        data = D ,
        subset = D[ , paste( "n2M" , n.M-1 , sep = "" ) ] == 1  )

    S1 <- termplot(
        model = cox1 ,
        plot = FALSE ,
        se = TRUE )[[1]]

    S1$lcl <- S1$y - 1.96 * S1$se
    S1$ucl <- S1$y + 1.96 * S1$se

    cox2 <- coxph(
        formula = F ,
        data = D ,
        subset = D[ , paste( "n2M" , n.M-1 , sep = "" ) ] == 2  )

    S2 <- termplot(
        model = cox2 ,
        plot = FALSE ,
        se = TRUE )[[1]]

    S2$lcl <- S2$y - 1.96 * S2$se
    S2$ucl <- S2$y + 1.96 * S2$se

    
      # When the Predictor is not ranked -> cut outliers
    if ( ptrans != "q" ) {
        
        PRange <- quantile(
            x = D[ , paste( ptrans , "P" , n.P-1 , sep = "" ) ] ,
            probs = c(0.01,0.99) ,
            na.rm = TRUE )
        
        S1 <- S1[ which( (S1$x > PRange[1]) & (S1$x < PRange[2])) ,]
        S2 <- S2[ which( (S2$x > PRange[1]) & (S2$x < PRange[2])) ,]
    }
    

    LowerYlim <- 1/round( exp( -min( c(S1$lcl,S2$lcl))))
    UpperYlim <- round( exp( max( c(S1$ucl,S2$ucl))))
    
    AtYaxis <- c(0.125,0.25,0.5,1,2,4,8)
    AtYaxis <- AtYaxis[
        which(
            (AtYaxis >= LowerYlim) &
            (AtYaxis <= UpperYlim))]
    
    plot(
        0 ,
        type = "n" ,
        bty = "n" ,
        axes = FALSE ,
        main = "" ,
        xlab = xlab ,
        ylab = "Partial hazard" ,
        xlim = range( c(S1$x,S2$x) ) ,
        ylim = range( log(AtYaxis)) )

    
    axis(1)
    axis(2 , at = log(AtYaxis) , label = AtYaxis )


      # confidence level shade for low modifier group
    ShadeX1 <- c( S1$x , S1$x[length(S1$x):1] )
    ShadeY1 <- c( S1$lcl , S1$ucl[length(S1$x):1] )

    polygon(
        x = ShadeX1 ,
        y = ShadeY1 ,
        border = FALSE ,
        col = "#D95F0222" )


      # confidence level shade for high modifier group
    ShadeX2 <- c( S2$x , S2$x[length(S2$x):1] )
    ShadeY2 <- c( S2$lcl , S2$ucl[length(S2$x):1] )

    polygon(
        x = ShadeX2 ,
        y = ShadeY2 ,
        border = FALSE ,
        col = "#7570B322" )
    
    abline( h = 0 , lty = 3 , lwd = 3 , col = "#FFFFFF" )

    lines(
        formula = y ~ x ,
        data = S1 ,
        lwd = 3 ,
        col = "#FFFFFF" )

    lines(
        formula = y ~ x ,
        data = S2 ,
        lwd = 3 ,
        col = "#FFFFFF" )
    
    lines(
        formula = y ~ x ,
        data = S1 ,
        lwd = 2 ,
        col = "#D95F0288" )

    lines(
        formula = y ~ x ,
        data = S2 ,
        lwd = 2 ,
        col = "#7570B388" )

    legend(
        "top" ,
        bty = "n" ,
        horiz = TRUE , 
        col = c("#D95F0288","#7570B388") ,
        lwd = 5 ,
        legend = c(
            paste( "Low" , sapply( JSON$Modifiers , with , Label )[n.M] ) ,
            paste( "High" , sapply( JSON$Modifiers , with , Label )[n.M] )))
        
}
