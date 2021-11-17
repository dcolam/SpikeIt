#' spikeIt.app
#'
#' Launches the pupillometry app.
#'
#' @export
# spikeIt.app <- function(){
#   library(shiny)
#   shinyApp(app.ui(), app.server())
# }


find_peaks <- function (x, m=5, thresh=0.5, dfThresh=0.5){
  
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
  })
  pks <- unlist(pks)
  baseline <- mean(x)
  pks <- sapply(pks, FUN=function(i){
    t <- abs(x[i]-baseline) / baseline
    #print(t)
    if(( t >= thresh / 100) & x[i] >=dfThresh) return(i) else return(numeric(0))
  })
  pks <- unlist(pks)
  pks
}

find_minima <- function (x, m = 3, thresh=5, lo){
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
  })
  pks <- unlist(pks)
  m <- mean(x)
  
  if(thresh != 0){
    pks <- sapply(pks, FUN=function(i){
      if(min(lo$x)<i & max(lo$x)>i){
        baseline <- predict(lo, i)
        #print(((x[i]-baseline)/baseline))
        if(((x[i]-baseline)/baseline) >= thresh) return(i) else return(numeric(0))
      }
      else return(numeric(0))
    })
    pks <- unlist(pks) 
  }
  pks
}


peakAnalysis <- function(x, dat, input, show.plots=F){
  # print(x)
  # dat <- tables$tabs[[x]]
  

  plotCols <- ifelse(show.plots, input$plotCols, input$plotCols2)
  background <- ifelse(show.plots, input$background, input$background2)
  
  stringency <- ifelse(show.plots, input$stringency, input$stringency2)
  dfThresh <- ifelse(show.plots, input$dfThresh, input$dfThresh2)
  
 # print(dfThresh)
  
  
  # print(dat)
  t <- dat$X1
  dat <- dat[, grepl(plotCols, colnames(dat))]
  
  #t <- dat$X1
  #dat$X1 <- NULL
  #print(background)
  if(!is.null(background)){
    #abs(x- mean(x)) / mean(x)
    #print(input$background)
    dat <- abs(dat - dat[,background]) / mean(dat[,background])
    dat <- dat[,colnames(dat) !=background]
  }
  #print(dat)
  
  
  
  maxima <- apply(dat,2, FUN=function(x) {
    
    
    #print(x)
    snr <- mean(x) / sd(x)
    #print(snr)
    #print(mean(x))
    max <- find_peaks(x, stringency, snr, dfThresh)
    #print(max)
  })
  
  minima <- apply(dat,2, FUN=function(x) {
    
    
    #print(x)
    snr <- mean(x) / sd(x)
    #print(snr)
    #print(mean(x))
    min <- find_minima(-x, 2, 0)
    #print(max)
  })
  
  
  dat$t <- t
  
  dat <- melt(dat, id.vars = "t", value.name = "Value")
  dat <- subset(dat, variable != "X1")
  dat$Table <- x
  
  
  # print("Max")
  # print(maxima)
  datMax <- data.frame()
  for (k in colnames(dat)) datMax[[k]] <- as.numeric()
  
  # print("Max")
  # print(datMax)
  
  datMax$start <- as.numeric()
  datMax$stop <- as.numeric()
  datMax$intensity.start <- as.numeric()
  datMax$intensity.stop <- as.numeric()
  datMax$total.duration <- as.numeric()
  datMax$duration.to.peak <- as.numeric()
  datMax$time.decay <- as.numeric()
  
  datMax$baseline <- as.numeric()
  datMax$amplitude <- as.numeric()
  
  # print("length")
  # 
  
  # print(minima)
  
  
  if(length(maxima) !=0) {
  #datMax <- data.frame()
  #for (k in colnames(dat)) datMax[[k]] <- as.numeric()
  for(x in names(maxima)){
    #print(x)
    datMax <- rbind(datMax, subset(dat, variable == x & t %in% maxima[[x]]))
    
  }
  
  datMin <- data.frame()
  for (k in colnames(dat)) datMin[[k]] <- as.numeric()
  for(x in names(minima)){
    #print(x)
    datMin <- rbind(datMin, subset(dat, variable == x & t %in% minima[[x]]))
    
  }
  
  # print(length(maxima))
  # print(maxima)
  # print(datMax)
  # #print(dat)
  # # 
  # print("Test2")
  # print(datMax)
  datMax$start <- NA
  datMax$stop <- NA
  datMax$intensity.start <- NA
  datMax$intensity.stop <- NA
  
  # 
  # print(datMax)
  # print("Test3")
  
  for(m in unique(datMax$variable)){
    
    #print("Test3")
    minima <- datMin[datMin$variable == m,]$t
    maxima <- datMax[datMax$variable ==m,]$t
    
    
    #print("Test4")
    if(min(maxima)< min(minima) & length(maxima)>1){
      maxima <- maxima[2:length(maxima)]
    }
    if(max(maxima)> max(minima)& length(maxima)>1){
      maxima <- maxima[1:length(maxima)-1]
    }
    
    #print("Test5")
    if(length(maxima) > 1){
      for(i in 1:length(maxima)){
        
        #print("Test6")
        #print(max(minima[which(minima<maxima[i])]))
        #print(min(minima[which(minima>maxima[i])]))
        start <- max(minima[which(minima<maxima[i])])
        stop <- min(minima[which(minima>maxima[i])])
        datMax[datMax$variable ==m,]$start[i] <- start
        datMax[datMax$variable ==m,]$intensity.start[i] <- subset(dat, variable == m & t ==start)$Value
        
        
        datMax[datMax$variable ==m,]$stop[i] <- stop
        datMax[datMax$variable ==m,]$intensity.stop[i] <- subset(dat, variable == m & t ==stop)$Value
        
        
      }
      #print("Test7")
      del <- 100
      datMax$total.duration <- del* (datMax$stop - datMax$start)
      datMax$duration.to.peak <- del* (datMax$t - datMax$start)
      datMax$time.decay <- del* (datMax$stop - datMax$t)
      
      datMax$baseline <- rowMeans(datMax[,c("intensity.start", "intensity.stop")])
      datMax$amplitude <- abs(datMax$Value - abs(mean(datMax$baseline, na.rm = T))) / abs(mean(datMax$baseline, na.rm = T))
      #print("Test8")
      #datMax[datMax$variable ==m,]$intensity.start <- subset(dat, variable == m & t %in% datMax[datMax$variable ==m,]$start)$value
      #datMax[datMax$variable ==m,]$intensity.stop <- subset(dat, variable == m & t %in% datMax[datMax$variable ==m,]$stop)$value
      #print(length(subset(dat, variable == m & t %in% datMax[datMax$variable ==m,]$start)$value))
      #print(length(datMax[datMax$variable ==m,]$start))
    }
  }
  }else{
    #print("HEllo")
    vars <- unique(dat$variable)
    #print(vars)
    for(m in vars){
      #print(which(m == vars))
      datMax[which(m == vars),] <- NA
      datMax[which(m == vars),]$variable <- m
      datMax[which(m == vars),]$Table <- x
    }
    #print(datMax)
    
  }
  ifelse(show.plots, return(list(dat=dat,datMax=datMax)), return(datMax))
  }
  
