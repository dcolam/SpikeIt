#' spikeIt.app
#'
#' Launches the pupillometry app.
#'
#' @export
# spikeIt.app <- function(){
#   library(shiny)
#   shinyApp(app.ui(), app.server())
# }

library(dplyr)

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
    #t <- abs(x[i]-baseline) / baseline
    #print(t)
    if(( x[i] >= thresh / 100) & x[i] >=dfThresh) return(i) else return(numeric(0))
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

  plotCols <- ifelse(show.plots, input$plotCols, input$plotCols2)
  
  stringency <- ifelse(show.plots, input$stringency, input$stringency2)
  dfThresh <- ifelse(show.plots, input$dfThresh, input$dfThresh2)
  del <- ifelse(show.plots, input$del, input$del2)

  t <- as.numeric(dat$t)
  

  dat <- dat[, grepl(plotCols, colnames(dat))]
  dat <- as.data.frame(apply(dat, 2, FUN=function(x){
    
    #baseline <- mean(min(x[1:100]))
    abs(x-  median(x))  / median(x)
    
  }))
  
  maxima <- apply(dat, 2, FUN=function(x) {
    
    snr <- mean(x) / sd(x)
    max <- find_peaks(x, stringency, snr, dfThresh)

  })
  
  minima <- apply(dat,2, FUN=function(x) {

    min <- find_minima(-x, 1, 0)

  })
  
  
  
  dat$t <- t
  
  melted.dat <- reshape2::melt(dat, id.vars = "t", value.name = "Value")
  melted.dat$variable <- as.character(melted.dat$variable)
  melted.dat$Table <- x
  melted.dat$Value <- as.numeric(melted.dat$Value)
  melted.dat$t <- as.numeric(melted.dat$t)

  datMax <- data.frame(variable=as.character(), Peak=as.numeric())
  
  for (k in names(maxima)){
    for(p in maxima[[k]]){
      datMax <- rbind(datMax, c(k, p))
    }}
  
  colnames(datMax) <- c("variable", "t")
  
  datMin <- data.frame(variable=as.character(), Peak=as.numeric())
  
  for (k in names(minima)){
    for(p in minima[[k]]){
      datMin <- rbind(datMin, c(k, p))
    }}
  
  colnames(datMin) <- c("variable", "t")
  
  
  melted.dat$t <- as.numeric(melted.dat$t)
  melted.dat$Value <- as.numeric(melted.dat$Value)
  

  if(nrow(datMax) !=0) {
    
    datMax <- merge(melted.dat, datMax, by=c("variable", "t"))
    datMin <- merge(melted.dat, datMin, by=c("variable", "t"))
    
    melted.dat$t <- as.numeric(melted.dat$t)
    datMax$t <- as.numeric(datMax$t)
    datMin$t <- as.numeric(datMin$t)
    
    datMax$start <- NA
    datMax$stop <-  NA
    datMax$intensity.start <-  NA
    datMax$intensity.stop <-  NA
    datMax$total.duration <-  NA
    datMax$duration.to.peak <-  NA
    datMax$time.decay <- NA
    datMax$baseline <-  NA
    datMax$amplitude <-  NA

    for(m in unique(datMax$variable)){

      minima <- as.numeric(datMin[datMin$variable == m,]$t)
      maxima <- as.numeric(datMax[datMax$variable ==m,]$t)
      
      minima <- minima[order(minima)]
      maxima <- maxima[order(maxima)]
      
      if(min(maxima) < min(minima) & length(maxima)>=1){
        maxima <- maxima[maxima > min(maxima)]
      }
      if(max(maxima) > max(minima) & length(maxima)>=1){
        maxima <- maxima[maxima < max(maxima)]
      }
      
      if(length(maxima) >= 1){
        for(i in 1:length(maxima)){
          
          t <- maxima[i]
          
          start <- max(minima[which(minima < maxima[i])])
          stop <- min(minima[which(minima >= maxima[i])])

          datMax[datMax$variable ==m & datMax$t == t,]$start <- start
          datMax[datMax$variable ==m & datMax$t == t,]$intensity.start <- subset(datMin, variable == m & t==start)$Value
          
          
          datMax[datMax$variable ==m & datMax$t == t,]$stop <- stop
          datMax[datMax$variable ==m & datMax$t == t,]$intensity.stop <- subset(datMin, variable == m & t==stop)$Value
          
          
        }
        
      }
    }
    
    #datMax$FrameNo <- datMax$t
    datMax$Time <- del*datMax$t / 1000
    
    datMax$total.duration <- del* (datMax$stop - datMax$start) / 1000
    datMax$duration.to.peak <- del* (datMax$t - datMax$start)/ 1000
    datMax$time.decay <- del* (datMax$stop - datMax$t)/ 1000
    
    datMax$start.Time <- del * datMax$start / 1000
    datMax$stop.Time <- del * datMax$stop / 1000
    
    datMax$baseline <- rowMeans(datMax[,c("intensity.start", "intensity.stop")])
    datMax$amplitude <- abs(datMax$Value - datMax$baseline) / abs(mean(datMax$baseline, na.rm = T))
    datMax <- datMax[complete.cases(datMax),]
    #datMax$t <- NULL
  
  }else{
    
    #datMax$FrameNo <- as.numeric()
    datMax$Value <-  as.numeric()
    datMax$Table <- as.character()
    #datMax$Time <- as.numeric()

    
    datMax$start <- as.numeric()
    datMax$stop <-  as.numeric()
    datMax$intensity.start <-  as.numeric()
    datMax$intensity.stop <-  as.numeric()
    datMax$total.duration <-  as.numeric()
    datMax$duration.to.peak <-  as.numeric()
    datMax$time.decay <-  as.numeric()
    
    datMax$baseline <-  as.numeric()
    datMax$amplitude <-  as.numeric()
    datMax$Time <-  as.numeric()
    datMax$start.Time <- as.numeric()
    datMax$stop.Time <-  as.numeric()
    
    datMax[1,] <- NA
    datMax$Table <- x
    

  }
  
  #melted.dat$FrameNo <- melted.dat$t
  melted.dat$Time <- del*melted.dat$t / 1000
  #melted.dat$t <- NULL
  
  if(nrow(datMax) ==0){
    datMax[1,] <- NA
    datMax$Table <- x
  }
  
  ifelse(show.plots, return(list(dat=melted.dat, datMax=datMax)), return(datMax))
}

save.image() 

