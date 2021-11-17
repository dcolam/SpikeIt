#' app.server
#'
#' Server function for the pupillometry app. When using the app locally, use `exploreSQlite.app()` instead.
#'
#' @return A function.
#' @export
#app.server <- function(){
library(data.table)
library(reshape2)
library(dplyr)
library(ggplot2)
library(emmeans)
#library(RColorBrewer)


options(shiny.maxRequestSize=1000*1024^2)
function(input, output, session){
  
  
  observeEvent(input$sampleFileInput,{
    print(input$sampleFileInput)
    tables$filenames <- c(tables$filenames, input$sampleFileInput$name)
    tables$sizes <- c(tables$sizes, input$sampleFileInput$size)
    
    tables$tabs <- c(tables$tabs, lapply(input$sampleFileInput$datapath, FUN=function(x){
      x=data.frame(read_delim(x, ",", escape_double = FALSE, trim_ws = TRUE))
    }))
    
    names(tables$tabs) <- tables$filenames
    updateSelectInput(session, inputId = "preview_sample", choices=tables$filenames)
    updateSelectInput(session, inputId = "view_sample", choices=tables$filenames)
    
  }
  )
  
  tables <- reactiveValues(
    filenames= NULL,
    sizes = NULL,
    tabs = NULL
  )
  
  output$samples_info <- renderTable({
    cbind( Files=tables$filenames, Size = tables$sizes)
  })
  
  observeEvent(input$removesamples, {
    tables = NULL
  })
  
  output$tabs <- DT::renderDataTable({
    if(!is.null(input$preview_sample)){
      tab <- tables$tabs[[input$preview_sample]]
      DT::datatable(tab[,grepl(input$paraCols, colnames(tab))], options = list(pageLength = 20))
    }
  })
  
  output$column <- renderUI({
    if(!is.null(input$preview_sample)){
    
      cols <- colnames(tables$tabs[[input$preview_sample]])
      
      cols <- unique(sapply(cols, FUN=function(x) gsub('[[:digit:]]+', '', x)))
      cols <- cols[cols != "X"]
      selectInput("paraCols", "Choose the Columns you want to display", choices=cols)
      
      
      }
  })
  
  output$plotcolumn <- renderUI({
    if(!is.null(input$view_sample)){
      
      cols <- colnames(tables$tabs[[input$view_sample]])
      
      cols <- unique(sapply(cols, FUN=function(x) gsub('[[:digit:]]+', '', x)))
      cols <- cols[cols != "X"]
      selectInput("plotCols", "Choose the Columns you want to display", choices=cols, selected = "Mean")
      
      
    }
  })
  
  output$cells <- renderUI({
    if(!is.null(input$plotCols)){
      
      cols <- colnames(tables$tabs[[input$view_sample]])
      
      cols <- cols[grepl(input$plotCols, cols)]
      
      cols <- gsub("([0-9]+).*$", "\\1", cols)
      names(cols) <- paste("Trace ", cols, sep="")
      
      
      selectizeInput("plotCells", "Choose the Cells you want to display", choices=cols, multiple=T, selected=cols)
      
      
    }
  })
  
  output$background <- renderUI({
    if(!is.null(input$plotCells) & !is.null(input$plotCols)){
      
      cols <- colnames(tables$tabs[[input$view_sample]])
      
      #cols <- cols[grepl(input$plotCells, cols)]
      cols <- cols[grepl(input$plotCols, cols)]
      cols <- cols[input$plotCells %in% cols]
      
      
      cols <- gsub("([0-9]+).*$", "\\1", cols)
      names(cols) <- paste("Trace ", cols, sep="")
      
      
      selectInput("background", "Choose the Background Trace to Subtract", choices=cols, selected=cols[1], multiple=F)
      
      
    }
  })
  
  
  
  p <- reactive({
    if(!is.null(input$plotCells) & !is.null(input$background) & !is.null(input$plotCols)){
     
      # dat <- tables$tabs[[input$view_sample]]
      # 
      # dat <- dat[, c("X1", input$plotCells)]
      # t <- dat$X1
      # dat$X1 <- NULL
      # 
      # if(!is.null(input$background)){
      #   #abs(x- mean(x)) / mean(x)
      #   #print(input$background)
      #   dat <- abs(dat[,input$plotCells] - dat[,input$background]) / mean(dat[,input$background])
      #   dat <- dat[,colnames(dat) != input$background]
      # }
      # #print(dat)
      # 
      # snr <- mean(rowMeans(dat)) / sd(rowMeans(dat))
      # 
      # maxima <- apply(dat,2, FUN=function(x) {
      #   
      #   
      #   #print(x)
      #   snr <- mean(x) / sd(x)
      #   #print(snr)
      #   #print(mean(x))
      #   max <- find_peaks(x, input$stringency, snr, input$dfThresh)
      #   #print(max)
      # })
      # 
      # minima <- apply(dat,2, FUN=function(x) {
      #   
      #   
      #   #print(x)
      #   snr <- mean(x) / sd(x)
      #   #print(snr)
      #   #print(mean(x))
      #   min <- find_minima(-x, 2, 0)
      #   #print(max)
      # })
      # 
      # 
      # #print(maxima)
      # dat$t <- t
      # 
      # dat <- melt(dat, id.vars = "t")
      # dat <- subset(dat, variable != "X1")
      # 
      # datMax <- data.frame()
      # for (k in colnames(dat)) datMax[[k]] <- as.numeric()
      # for(x in names(maxima)){
      #   #print(x)
      #   datMax <- rbind(datMax, subset(dat, variable == x & t %in% maxima[[x]]))
      #   
      # }
      # 
      # datMin <- data.frame()
      # for (k in colnames(dat)) datMin[[k]] <- as.numeric()
      # for(x in names(minima)){
      #   #print(x)
      #   datMin <- rbind(datMin, subset(dat, variable == x & t %in% minima[[x]]))
      #   
      # }
      # 
      # #print(length(maxima))
      # #print(maxima)
      # #print(datMin)
      # #print(datMax)
      # #print(dat)
      # 
      # 
      # datMax$start <- NA
      # datMax$stop <- NA
      # datMax$intensity.start <- NA
      # datMax$intensity.stop <- NA
      # 
      # for(m in unique(datMax$variable)){
      #   
      #   
      #   minima <- datMin[datMin$variable == m,]$t
      #   maxima <- datMax[datMax$variable ==m,]$t
      #   
      #   if(min(maxima)< min(minima) & length(maxima)>1){
      #     maxima <- maxima[2:length(maxima)]
      #   }
      #   if(max(maxima)> max(minima)& length(maxima)>1){
      #     maxima <- maxima[1:length(maxima)-1]
      #   }
      #   
      #   if(length(maxima) > 1){
      #     for(i in 1:length(maxima)){
      #       
      #       
      #       #print(max(minima[which(minima<maxima[i])]))
      #       #print(min(minima[which(minima>maxima[i])]))
      #       start <- max(minima[which(minima<maxima[i])])
      #       stop <- min(minima[which(minima>maxima[i])])
      #       datMax[datMax$variable ==m,]$start[i] <- start
      #       datMax[datMax$variable ==m,]$intensity.start[i] <- subset(dat, variable == m & t ==start)$value
      #       
      #       
      #       datMax[datMax$variable ==m,]$stop[i] <- stop
      #       datMax[datMax$variable ==m,]$intensity.stop[i] <- subset(dat, variable == m & t ==stop)$value
      #       
      #       
      #     }
      #     
      #     #datMax[datMax$variable ==m,]$intensity.start <- subset(dat, variable == m & t %in% datMax[datMax$variable ==m,]$start)$value
      #     #datMax[datMax$variable ==m,]$intensity.stop <- subset(dat, variable == m & t %in% datMax[datMax$variable ==m,]$stop)$value
      #     #print(length(subset(dat, variable == m & t %in% datMax[datMax$variable ==m,]$start)$value))
      #     #print(length(datMax[datMax$variable ==m,]$start))
      #     }
      # }
      # 
      # print(datMax)
      
      #print(input$checkFacetgrid)
      
      #print(input[["view_sample"]])
      
      #dat <- tables$tabs[[input$view_sample]]
      dat <- tables$tabs[[input$view_sample]]
       
      dat <- dat[, c("X1", input$plotCells)]
      print("Print dat")
      print(dat)
      print(peakAnalysis)
      peaks <- peakAnalysis(input$view_sample, dat, input, show.plots=T)
      dat <- peaks$dat
      datMax <- peaks$datMax
      #print(peaks)
      
     
      
      p <- ggplot(data=dat, aes(x=t, y=Value, colour = variable)) + geom_line() 
      
      if(input$show.peaks){
        p <- p +
        geom_point(data=datMax, aes(x=t, y=Value, color=variable)) +
        geom_point(data=datMax, aes(x=start, y=intensity.start, label="Peak Start"),  color="red") +
        geom_point(data=datMax, aes(x=stop, y=intensity.stop), label="Peak Stop", color="blue") + labs(x = "Time", y = "dF/F", color = "Trace")
      }
      
      # p <- ggplot(data=dat, aes(x=as.numeric(t), y=as.numeric(Value), color = variable)) + geom_line() +
      #   geom_point(data=datMax, aes(x=as.numeric(t), y=as.numeric(Value), color=variable)) + xlab("Time") + ylab("dF/F") +
      #   geom_point(data=datMax, aes(x=as.numeric(start), y=as.numeric(intensity.start))) +
      #   geom_point(data=datMax, aes(x=as.numeric(stop), y=as.numeric(intensity.stop)))
      
      
      
      if(input$checkFacetgrid){
        p <- p + facet_grid(variable~., scales="free")
      }
      
      #+ facet_grid(variable~., scales="free")
      return(p)
      
    }
    
  })
  
  
  output$traces <- renderPlot({
    
    p()
    
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      gsub(".csv", ".pdf", input$view_sample)
    },
    content = function(file){

      ggsave(file, plot = p(), device="pdf", width=input$plotWidth, height = input$plotHeight, units = "in")
    }
  )
  
  
  output$plotcolumn2 <- renderUI({
    if(!is.null(tables$tabs)){
      cols <- lapply(tables$tabs, FUN= function(x){
        #print(x)
        colnames(x)
      })
      cols <- unique(unlist(cols))
      cols <- unique(sapply(cols, FUN=function(x) gsub('[[:digit:]]+', '', x)))
      cols <- cols[cols != "X"]
      selectInput("plotCols2", "Choose the Columns you want to display", choices=cols, selected = "Mean")
      
    }
    
  })
  
  output$background2 <- renderUI({
    
      if(!is.null(tables$tabs)){
      cols <- lapply(tables$tabs, FUN= function(x){
        #print(x)
        colnames(x)
      })
      
      #print(cols)
      cols <- unique(unlist(cols))
      
      cols <- cols[grepl(input$plotCols2, cols)]
      
      cols <- gsub("([0-9]+).*$", "\\1", cols)
      names(cols) <- paste("Trace ", cols, sep="")
      
      
      selectInput("background2", "Choose the Background Trace to Subtract", choices=cols, selected=cols[1], multiple=F)
      }
  })
  
  
  observeEvent(input$analysis, {
    
    maxTables <- lapply(names(tables$tabs), FUN=function(x){
      dat <- tables$tabs[[x]]
      x= peakAnalysis(x, dat, input, show.plots=F)
    }) 
    maxTables <- bind_rows(maxTables, .id = "column_label")
    #print(maxTables)
    colnames(maxTables) <- c("Table.ID", "Peak.Time", "Cell.Trace", "Peak.Intensity", "Table", "Peak.Start", "Peak.Stop", "Start.Intensity", "Stop.Intensity", 
                             "Peak.Duration", "Duration.To.Peak", "Time.To.Decay", "Baseline.Intensity", "Amplitude")
    #print(maxTables)
    tables$peakTables <- maxTables
    tables$meanTables <- aggregate(maxTables, by=list(Table.ID = maxTables$Table.ID, 
                                                      Cell.Trace = maxTables$Cell.Trace, 
                                                      Table=maxTables$Table), 
                                   FUN=mean)
    tables$meanTables <-  tables$meanTables[, colSums(is.na(tables$meanTables)) != nrow( tables$meanTables)]
    #print(tables$meanTables)
  })
  
  # observeEvent(tables, {
  #   if(!is.null(tables[[input$resultsTable]])){
  #   updateSelectizeInput("resultsColumns", choices=colnames(tables[[input$resultsTable]]), value="Amplitude")
  #   }
  # })
  
  output$results <- DT::renderDataTable({
    if(!is.null(tables[[input$resultsTable]])){
      #tab <- tables$tabs[[input$preview_sample]]
      DT::datatable(tables[[input$resultsTable]], options = list(pageLength = 20))
    }
  })
  
  
  
  output$plotResults <- renderPlot({
    
    
    if(!is.null(tables[[input$resultsTable]])){
      print(input$resultsColumns)
      dat <- melt(tables[[input$resultsTable]], id.vars=c("Cell.Trace", "Table"), measure.vars=input$resultsColumns)
      
      if(input$resultsTable == "peakTables"){
      
      p <- ggplot(dat, aes(x=Cell.Trace, y=value)) + geom_boxplot() + facet_grid(variable ~ Table, scales="free")
      
      } else{
        p <- ggplot(dat, aes(x=Table, y=value)) + geom_boxplot() + facet_grid(variable ~ ., scales="free")
        
        
      }
      p
    }
  })
  
  
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste(input$download_table, ' Edited Table.csv', sep='')
    },
    content = function(file) {
      write.csv(tables[[input$download_table]], file, row.names = FALSE)
    }
  )
  
  
}
#}
  