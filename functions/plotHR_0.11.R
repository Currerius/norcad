    # Rforge blog
    # http://rforge.org/2009/10/30/plot-function-for-additive-cox-proportional-hazard-regression/
    # Author:
    # Reinhard Seifert, biostatistician at Haukeland University Hospital Bergen, Norway.
     
    ## 0.7 is a complete rewrite of the code:
    ## -  the local dataset is trimmed to include only model covariates and complete obs only
    ## -  the fit and CI's for the plot are now attached to the local dataset
    ## -  x.log removed -> later xlog from plot.default() will be used
    ## -  xlim, ylim estimated by plot.default() and not manually from fivenum(covariate)
    ## -  density plot rewritten
     
    ## 0.8 rewrite of the 'rug = "density"' option:
    ## - fixes a bug with the scale of the densityplot
    ## - removed the "Quartile 1" , "Median", "Quartile 3"
    ## white lines (backgroundcolor) through both densityplot and confidence shade denote 1Q, median, 3Q.
     
    ## 0.9
    ## - set the rug = "density" option as default
    ## - moved the density-plot right on top of the x-axis

    ## 0.11
    ## Max Gordon rewrote big parts of the function:
    ##            - the density polygon is now plotted in the same canvas
    ##            - 'terms' can now also be specified with the covariate name string
    ##            - plotHR should work now with cph() from Harrel's library(rms)
    ##            - interaction terms in the model are removed

    plotHR <- function (model , terms = 1 , se = TRUE , col.se = "#DEEBF7",
                        rug = "density" , col.dens = grey(0.8) , probs = c(0.025,0.25,0.50,0.75,0.975) ,
                        ref = TRUE , col.ref = "white" , lty.ref = 1 , lwd.ref = 1 ,
                        xlab = "" , ylab = "Hazard Ratio" , main = NULL ,
                        xlim = NULL , ylim = NULL,
                        col.term = "#08519C", lwd.term = 3, digits = 1,
                        cex = 1 , bty = "n" , axes = TRUE) {
  
       
       
        ## extract the names of all model covariates
        all.labels <- attr(model$terms , "term.labels")
       
        # remove 'strata()' / 'factor()' / "pspline( , df = a )"
        all.labels <- sub ( "pspline[(]([a-zA-Z._0-9]*)(|[, ]*(df|nterm)[ ]*=[ ]*[0-9a-zA-Z_]+)[)]" , "\\1" , all.labels)
        all.labels <- sub ( "factor[(]([a-zA-Z._0-9]*)[)]" , "\\1" , all.labels)
        all.labels <- sub ( "strata[(]([a-zA-Z._0-9]*)[)]" , "\\1" , all.labels)
     
        # The cph in the Design package uses rcs instead of pspline
        all.labels <- sub ( "rcs[(]([a-zA-Z._0-9]*)(|([, ]*([0-9]+|cut[ ]*=[ ]*c[(][0-9a-zA-Z_, ]+[)])))[)]" , "\\1" , all.labels)
       
        # Allow the term searched for be a string
        if (is.character(terms)){
            terms <- grep(terms, all.labels)
            if(length(terms) == 0){
                stop(paste("Could not find term:", terms))
            }
        }
       
        # pick the name of the main term which is goint to be plotted
        term.label <- all.labels[terms]
        if (is.na(term.label)){
            stop(paste("Term", terms, "not found"))
        }
       
        # Remove interaction terms since the data can't be found, ex. male_gender:prosthesis
        terms_with_interaction <- grep("[_.a-zA-Z0-9]+:[_.a-zA-Z0-9]+", all.labels)
        if(length(terms_with_interaction)>0){
            all.labels <- all.labels[!(terms_with_interaction == 1:length(all.labels))]
        }
       
        ## extract data from model;
        # only the covariates really used in the model
        # only complete covariate records (only used in the model anyway)
        # 'as.data.frame()' and 'names()' have to be explicitly specified in case of a univariate model
        data <- eval(model$call$data)
        data <- as.data.frame(na.exclude(data[ , all.labels]))
        names(data) <- all.labels
       
       
        ## get the quartiles of the main term
        quantiles <- quantile(data[,term.label] , probs = probs , na.rm = TRUE)
       
       
        ### _______________ the smooth term prediction ____________
        ## calculate the HR for all the covariate values found in the dataset
        if(length(grep("cph", model)) > 0){
            # If this is a cph model then don't exclude the na values
            term <- predict (model , type="terms" , se.fit = TRUE , terms = terms, expand.na=FALSE, na.action=na.pass)
            # attach the smooth fit for the HR ('fit') and the CI's to the dataset
            data$fit <- as.numeric(term$fit)
            data$ucl <- as.numeric(term$fit + 1.96 * term$se.fit)
            data$lcl <- as.numeric(term$fit - 1.96 * term$se.fit)
        }else{
            term <- predict (model , type="terms" , se.fit = TRUE , terms = terms)
            # attach the smooth fit for the HR ('fit') and the CI's to the dataset
            data$fit <- as.numeric(na.exclude(term$fit))
            data$ucl <- as.numeric(na.exclude(term$fit + 1.96 * term$se.fit))
            data$lcl <- as.numeric(na.exclude(term$fit - 1.96 * term$se.fit))
        }
        ### _______________ this is the main extraction __________
       
       
       
        ### _______________ what now follows is the graphical manipulation of the plots ______

        
        # set plotting parameters
        par(las = 1 , cex = cex)
        if (is.null(ylim)) ylim <- exp(range(data$fit , na.rm = FALSE))

        
        # plot empty plot with coordinatesystem and labels
        plot( data$fit ~ data[ , term.label] , xlim = xlim , ylim = round( log(ylim) , digits = 1) , xlab = xlab , ylab = ylab , main = main, type = "n" , axes = FALSE )

        
        # sorting indices for ploting
        i.backw <- order(data[,term.label] , decreasing = TRUE)
        i.forw <- order(data[,term.label])
       
       
        # plot CI as polygon shade - if 'se = TRUE' (default)
        if (se) {
            x.poly <- c(data[,term.label][i.forw] , data[,term.label][i.backw])
            y.poly <- c(data$ucl[i.forw] , data$lcl[i.backw])
            polygon(x.poly , y.poly , col = col.se , border = NA)
        }
       

        ### _______________ rug = "density" ____________________________________________________
        ### density plot at bottom of panel if rug = "density" in function call

        if (rug == "density") {
          
          # calculate the coordinates of the density function

          density <- density( data[,term.label] )

          # the height of the densityity curve

          max.density <- max(density$y)

          # Get the boundaries of the plot to
          # put the density polygon at the x-line

          plot_coordinates <- par("usr")
        
          # get the "length" and range of the y-axis
          y.scale <- plot_coordinates[4] - plot_coordinates[3]
        
          # transform the y-coordinates of the density
          # to the lower 10% of the plotting panel
          
          density$y <- (0.1 * y.scale / max.density) * density$y + plot_coordinates[3]
        
          ## plot the polygon

          polygon( density$x , density$y , border = F , col = col.dens) }

        
        # plot white lines (background color) for 2.5%tile, 1Q, median, 3Q and 97.5%tile through confidence shade and density plot
        axis( side = 1 , at = quantiles , labels = FALSE , lwd = 0 , col.ticks = "white"  , lwd.ticks = 1 , tck = 1 )
        # plot white line ( background color ) for HR = 1 reference
        if (ref) {
          axis( side = 2 , at = 0 , labels = FALSE , lwd = 0 , col.ticks = col.ref , lwd.ticks = lwd.ref , tck = 1 )}

       
        ### _______________ rug = "ticks" ____________________________________________________
        ### rug plot if "ticks" is specified in function call
        if (rug == "ticks") {
            # rugs at datapoints
            axis(side = 1 , line = -1.2 , at = jitter(data[,term.label]) , labels = F , tick = T , tcl = 0.8 , lwd.ticks = 0.1 , lwd = 0)
            # rugs and labels at 1Q, median and 3Q
            axis(side = 1 , line = -1.0 , at = fivenum(data[,term.label])[2:4], lwd = 0 , tick = T, tcl = 1.2 , lwd.ticks = 1 , col.ticks = "black" , labels = c("Quartile 1","Median","Quartile 3"), cex.axis = 0.7, col.axis = "black" , padj = -2.8)
            axis(side = 1 , line = 0.0 , at = fivenum(data[,term.label])[2:4], lwd = 0 , tick = T, tcl = 0.2 , lwd.ticks = 1 , col.ticks = "black", labels = FALSE)
        }
       
       
        # last but not least the main plotting line for the smooth estimate:
        # ___________ main plot _____________
        lines(data[,term.label][i.forw] , data$fit[i.forw], col = col.term, lwd = lwd.term)
        # ___________ main plot _____________
       
       
        # plot the axes
        if (axes){
            axis(side = 1)
            axis(side = 2 , at = axTicks(2) , label = round( exp(axTicks(2)) , digits = digits))
        }
       
       
        # plot a box around plotting panel if specified - not plotted by default
        box(bty = bty)
    }
