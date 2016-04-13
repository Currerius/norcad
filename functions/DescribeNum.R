DescribeNum <- function(measure) {
    
    COUNTS <- as.character(
        c(
            n = sum( !is.na(D[,paste(measure)])) ,
            missing = sum( is.na(D[,paste(measure)])) ,
            unique = length( unique( na.exclude( D[,paste(measure)] )))))
    
    PARAMETERS <- as.character(
        signif(
            c(
                mean = mean(D[,paste(measure)] , na.rm = TRUE ) ,
                sd = sd(D[,paste(measure)] , na.rm = TRUE )) ,
            digits = 3 ))
    
    QUANTILES <- as.character(
        signif(
            quantile(
                D[,paste(measure)] ,
                na.rm = TRUE ,
                probs = c(0,0.05,0.10,0.25,0.50,0.75,0.90,0.95,1)) ,
            digits = 3 ))

    MAD <- as.character(
        signif(
            mad( D[,paste(measure)] , na.rm = TRUE ) ,
            digits = 3 ))

    OUT <- c(
        COUNTS ,
        PARAMETERS ,
        QUANTILES ,
        MAD )

    names(OUT) <- c("n","missing","unique","mean","sd","min","5%","10%","25%","median","75%","90%","95%","max","mad")

    return(OUT)}
