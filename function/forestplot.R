Estimates <- rbind(
    stratifiedCox( STRATUM = "sex" ) ,
    stratifiedCox( STRATUM = "AgeClass" ) ,
    stratifiedCox( STRATUM = "diabetes_mellitus" ) ,
    stratifiedCox( STRATUM = "current_smoking" ) ,
    stratifiedCox( STRATUM = "B2Class" ) ,
    stratifiedCox( STRATUM = "ApoBClass" ) ,
    stratifiedCox( STRATUM = "ApA1Class" )
    )

plot(
    x = seq(
        from = log(0.9) ,
        to = log(1.4) ,
        length.out = 14) ,
    y = 1:14 ,
    type = "n" ,
    axes = FALSE ,
    main = "" ,
    xlab = "AMI hazard ratios per\nSD increase in log(TML)" ,
    ylab = "")

points( Estimates$logHR , 14:1 , pch = 19)
segments( Estimates$LCL , 14:1 ,  Estimates$UCL , 14:1 )

axis(
    side = 1 ,
    at = log( seq(
        from = 0.9 ,
        to = 1.4 ,
        by = 0.1 )) ,
    labels = seq(
        from = 0.9 , 
        to = 1.4 ,
        by = 0.1 ))

abline( v = 0 , lty = 3 )

axis(
    side = 2 ,
    at = 14:1 ,
    lwd = 0 ,
    labels = c(
        expression("Women") ,
        expression("Men") ,
        expression("Age"<=62) ,
        expression("Age">62) ,
        expression("Non-diabetics") ,
        expression("Diabetics") ,
        expression("Non-smokers") ,
        expression("Smokers") ,
        expression("B2"<=11.2) ,
        expression("B2">11.2) ,
        expression("ApoB"<=0.87) ,
        expression("ApoB">0.87) ,
        expression("ApoA1"<=1.30) ,
        expression("ApoA1">1.30)))

axis(
    side = 4 ,
    at = 2*(7:1)-.5 ,
    lwd = 0 ,
    labels = Pvalues)
