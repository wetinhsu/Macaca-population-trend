
rm(list=ls())

## set directory
# setwd("C://Rdata/BirdIndex/")
#setwd("c:/R/AgriLand_frog/")

## load packages
usepackage <- c("reshape2", "R2jags", "MCMCvis", "ggplot2", "tidyverse")
install.packages(usepackage[!(usepackage %in% installed.packages()[,1])])
sapply(usepackage, library, character.only = TRUE)
# library(reshape2)
# library(R2jags)
# library(MCMCvis)
# library(ggplot2)

# input file (*.csv) and create output directory
f_name <- dir(pattern = "BBS_Monkey.*_0222.csv")
d_name <- f_name %>% gsub("(^.*).csv$", "\\1", x = .)
sapply(d_name, dir.create)

## for-loop by file #####
for (fi_name in f_name) {
  
  t0 <- Sys.time()
  
  # load site-year matrix
  data1<-read.csv(fi_name)
  Years<-ncol(data1)-1
  N.site<-nrow(data1)
  
  # set output directory
  di_name <- fi_name %>% gsub("(^.*).csv$", "\\1", x = .)
  setwd(di_name)
  
  # convert data into dataframe format
  data2<-data.frame(site=rep(seq_len(N.site), each=(Years)),
                    year=rep(1:Years, N.site),
                    count=NA)
  data.melt <- melt(data1, id.vars="site", measure.vars=colnames(data1)[-1])
  data.melt <- data.melt[order(data.melt$site, data.melt$variable),]
  data2$count <- data.melt$value
  
  # remove NAs
  data2 <- na.omit(data2)
  data2
  ### THIS IS THE DATA FORMAT NEEDED FOR THE MODEL ###
  ### SO IF YOU CAN PREPARE YOUR DATA IN THIS FORMAT, THE CODE ABOVE IS NOT RELEVANT ###
  
  
  # store data into variables to be used in model
  SITE<-data2$site
  YEAR<-data2$year
  Y<-data2$count
  N.data<-length(Y)
  
  ## define model -------------------------------------------
  cat("
    model
    {
      # define prior distribution for parameters
    	for(i in 1:3){
    	   tau[i] ~ dgamma(0.01, 0.01)
    	   sd[i] <- sqrt(1/tau[i])
    	}
    
    	yr[1] ~ dnorm(0, 0.001)
    	yr[2] ~ dnorm(0, 0.001)	
    	
    	# define overall year effect
    	for(i in 3:Years){
    	   m.yr[i] <- 2*yr[i-1] - yr[i-2]
    	   yr[i] ~ dnorm(m.yr[i], tau[1])
    	}
    	
    	# define site effect
    	for(i in 1:N.site){
    	   st[i] ~ dnorm(0, tau[2])
    	}    	
      
      
      # fit data to the model
    	for(i in 1:N.data){
    	
         # site-specific year effect
    	   yr.st[SITE[i], YEAR[i]] ~ dnorm(yr[YEAR[i]], tau[3])
         
         # log expected count equals site-specific year effect + site effect 
    	   log(lamda[SITE[i], YEAR[i]]) <- yr.st[SITE[i], YEAR[i]] + st[SITE[i]]
    	   
    	   # count derived from poisson distribution 
    	   Y[i] ~ dpois(lamda[SITE[i], YEAR[i]])
    	   
    	}
    	
    	# calculate index (index in the first year = 100)
    	for(i in 1:Years){	   
    	   ind[i] <- 100*(exp(yr[i]))/(exp(yr[1]))	   
    	}
    	      # calculate % changes over the last  10 years
    	chg[1] <- 100*(ind[Years] - ind[Years-3])/ind[Years-3]
      
    }
    ", file="model.txt")
  
  
  
  # Setting for running model
  data<-list("Years","N.site","N.data","Y","SITE","YEAR")
  parameters<-c("ind","chg","yr","sd","st")
  inits <- function() {
    list(yr=rnorm(Years, mean = 0, sd = 1), 
         tau=rgamma(3, shape=100, scale=0.01),
         st=rnorm(N.site, mean=0, sd=1)
    ) 
  }
  
  # Run model (this took about 1.5 mins on my laptop with this dataset)
  start <- Sys.time()
  res <- jags(data = data, inits = inits,
              parameters.to.save = parameters,
              model.file="model.txt",
              n.chains=3, n.iter=20000,
              n.burnin=10000, n.thin=4)
  
  print(Sys.time()-start)
  
  
  # Save summary result
  pdf("summary.pdf")
  plot(res)
  dev.off()
  write.csv(res$BUGSoutput$summary,"summary.csv")
  # check r-hat values in this file; r-hat < 1.1 for all parameters indicates model conversion
  
  # Save posterior samples (THIS COULD BE A BIG FILE)
  write.csv(MCMCchains(res),"sims.csv")
  
  # check trace plot; three chaines well mixed indicate model conversion
  MCMCtrace(res, open_pdf = FALSE) %>% suppressWarnings()
  dev.off()
  
  # draw index
  summaryindex <- MCMCsummary(res, params="ind")[,c("2.5%","50%","97.5%")]
  summaryindex$Year <- 2015:2021
  colnames(summaryindex)[1:3] <- c("LowerCI","Median","UpperCI")
  
  p <- ggplot(summaryindex, aes(Year)) + 
    geom_line(aes(y=Median), colour="red", linewidth=1.5) + 
    scale_x_continuous(breaks = c(2015:20217)) +
    geom_ribbon(aes(ymin=LowerCI, ymax=UpperCI), alpha=0.2) +
    ylab("Index (100 in 2015)") + theme_bw() + 
    theme(text = element_text(size = 18),
          aspect.ratio = 1) 
ggsave(filename = "index.pdf", plot = p,
       device = "pdf", width = 28, height = 16, units = "cm")
  
  # print estimated % changes over the last 30, 20, 10 years
  MCMCsummary(res, params="chg")[,c("2.5%","50%","97.5%")]

  ## print process ----
  
  
  d_t0 <- Sys.time() - t0
  cat(paste0(fi_name, " is finished. ",
             match(fi_name, f_name), "/", length(f_name), "\n",
             format(d_t0), "\n"
  ))
  rm(list = ls()[!ls() %in% c("f_name", "d_name")])
  
  # go up the directory
  setwd("..")
  
  
  
}


#####


