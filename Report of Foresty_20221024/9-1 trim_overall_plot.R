

#從github上copy來的原程式------------------------------------
plot.trim.overall <- function(x, imputed=TRUE, ...) {
  #browser()
  X <- x
  title <- if (is.null(list(...)$main)){
    attr(X, "title")
  } else {
    list(...)$main
  }
  
  tpt = X$timept
  J <- X$J
  
  # Collect all data for plotting: time-totals
  ydata <- X$tt
  
  # error bars
  y0 = ydata - X$err
  y1 = ydata + X$err
  
  trend.line <- NULL
  conf.band  <- NULL
  
  X$type <- "changept" # Hack for merging overall/changepts
  if (X$type=="normal") {
    # Trend line
    a <- X$coef[[1]][1] # intercept
    b <- X$coef[[1]][2] # slope
    x <- seq(1, J, length.out=100) # continue timepoint 1..J
    ytrend <- exp(a + b*x)
    xtrend <- seq(min(tpt), max(tpt), len=length(ytrend)) # continue year1..yearn
    trendline = cbind(xtrend, ytrend)
    
    # Confidence band
    xconf <- c(xtrend, rev(xtrend))
    alpha <- 0.05
    df <- J - 2
    t <- qt((1-alpha/2), df)
    j = 1:J
    dx2 <- (x-mean(j))^2
    sumdj2 <- sum((j-mean(j))^2)
    dy <- t * sqrt((X$SSR/(J-2))*(1/J + dx2/sumdj2))
    ylo <- exp(a + b*x - dy)
    yhi <- exp(a + b*x + dy)
    yconf <- c(ylo, rev(yhi))
    conf.band <- cbind(xconf, yconf)
  } else if (X$type=="changept") {
    nsegment = nrow(X$slope)
    for (i in 1:nsegment) {
      
      # Trend line
      a <- X$intercept[i,3]
      b <- X$slope[i,3]
      from <- which(tpt==X$slope[i,1]) # convert year -> time
      upto <- which(tpt==X$slope[i,2])
      delta = (upto-from)*10
      x      <- seq(from, upto, length.out=delta) # continue timepoint 1..J
      ytrend <- exp(a + b*x)
      xtrend <- seq(tpt[from], tpt[upto], length.out=length(ytrend))
      if (i==1) {
        trendline = cbind(xtrend, ytrend)
      } else {
        trendline = rbind(trendline, NA)
        trendline = rbind(trendline, cbind(xtrend, ytrend))
      }
      
      # Confidence band
      xconf <- c(xtrend, rev(xtrend))
      alpha <- 0.05 # Confidence level
      ntpt <- upto - from + 1 # Number of time points in segment
      df <- ntpt - 2
      if (df<=0) next # No confidence band for this segment...
      
      t <- qt((1-alpha/2), df)
      j = from : upto
      dx2 <- (x-mean(j))^2
      sumdj2 <- sum((j-mean(j))^2)
      SSR = X$SSR[i] # Get stored SSR as computed by overall()
      dy <- t * sqrt((SSR/df)*(1/ntpt + dx2/sumdj2))
      ylo <- exp(a + b*x - dy)
      yhi <- exp(a + b*x + dy)
      yconf <- c(ylo, rev(yhi))
      
      if (is.null(conf.band)) {
        conf.band <- cbind(xconf, yconf)
      } else {
        conf.band = rbind(conf.band, NA)
        conf.band = rbind(conf.band, cbind(xconf, yconf))
      }
      
    }
    yrange = c(300,700)
  } else stop("Can't happen")
  
  # Compute the total range of all plot elements (but limit the impact of the confidence band)
  xrange = range(trendline[,1], na.rm=TRUE)
  yrange1 = range(range(y0), range(y1), range(trendline[,2]), na.rm=TRUE)
  yrange2 = range(range(conf.band[,2], na.rm=TRUE))
  yrange = range(yrange1, yrange2, na.rm=TRUE)
  ylim = 2 * yrange1[2]
  if (yrange[2] > ylim) yrange[2] = ylim
  
  # Ensure y-axis starts at 0.0
  yrange <- range(0.0, yrange)
  
  # Now plot layer-by-layer (using ColorBrewer colors)
  cbred <- rgb(228,26,28, maxColorValue = 255)
  cbblue <- rgb(55,126,184, maxColorValue = 255)
  plot(xrange, yrange, type='n', xlab=" ", ylab="Count", las=1, main=title,...)
  polygon(conf.band, col=gray(0.9), lty=0)
  lines(trendline, col=cbred, lwd=3) # trendline
  segments(tpt,y0, tpt,y1, lwd=3, col=gray(0.5))
  points(tpt, ydata, col=cbblue, type='b', pch=16, lwd=3)
}

#===========================================
plot(overall(m1, "imputed"), axes = F)
axis(1, at = 2015:2022, cex.axis=1.5, xlab ="    ") #這裡把xlab偷改掉
axis(2, cex.axis=1.5)
box()
#title( "imputed" )

text(2021.7,105,"p >0.0.5", cex = 1.6)


