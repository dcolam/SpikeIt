#' app.server
#'
#' Server function for the pupillometry app. When using the app locally, use `exploreSQlite.app()` instead.
#'
#' @return A function.
#' @export
#app.server <- function(){
library(data.table)
library(DT)
library(reshape2)
library(dplyr)
library(ggplot2)
library(emmeans)
#library(RColorBrewer)


options(shiny.maxRequestSize=1000*1024^2)
function(input, output, session){
  
  
  observeEvent(input$sampleFileInput,{
    #print(input$sampleFileInput)
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
    tabs = NULL,
    peakTables = NULL
  )
  
  output$samples_info <- renderTable({
    if(!is.null(tables$sizes)){
    cbind( Files=tables$filenames, Size = utils:::format.object_size(tables$sizes, "auto"))
    }
  })
  
  # prints the menu badge for samples
  output$badgeText_samples <- renderText({
    if(is.null(tables$tabs)) return("empty")
    else(return("ready"))
  })
  
  output$badgeText_samples2 <- renderText({
    if(is.null(tables$peakTables)) return("empty")
    else(return("ready"))
  })
  

  
  observeEvent(input$removesamples, {
    tables$filenames <-  NULL
    tables$sizes <-  NULL
    tables$tabs <-  NULL
    tables$peakTables <- NULL
    #tables$peaks$dat <- NULL
    # tables$peaks$datMax <- NULL
  })
  
  observeEvent(input$testsamples, {
    
    exTab <- file.info("Example_Table.csv")
    exTab$filename <- "Example_Table.csv"
    tables$filenames <- c(tables$filenames, exTab$filename)
    tables$sizes <- c(tables$sizes, exTab$size)
    
    tables$tabs <- c(tables$tabs, lapply(exTab$filename, FUN=function(x){
      x=data.frame(read_delim(x, ",", escape_double = FALSE, trim_ws = TRUE))
    }))
    
    names(tables$tabs) <- tables$filenames
    updateSelectInput(session, inputId = "preview_sample", choices=tables$filenames)
    updateSelectInput(session, inputId = "view_sample", choices=tables$filenames)
    
    
  })
  
  output$tabs <- DT::renderDataTable({
    if(!is.null(input$preview_sample)){
      tab <- tables$tabs[[input$preview_sample]]
      cols <- grepl(input$paraCols, colnames(tab))
      DT::datatable(tab[,cols], options = list(pageLength = 20))
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
    if(!is.null(input$plotCells) & !is.null(input$plotCols)){
     
      dat <- tables$tabs[[input$view_sample]]
      dat$t <- row.names(dat)
      dat <- dat[, c("t", input$plotCells)]
      
      withProgress(message = 'Making plot', value = 0, {
      incProgress(0.334, detail = "Extracting Peaks..")
      tables$peaks <- peakAnalysis(input$view_sample, dat, input, show.plots=T)
      
      
      dat <- tables$peaks$dat
      datMax <- tables$peaks$datMax
      #print(peaks)
      #print(datMax)
      incProgress(0.667, detail = "Plotting..")
      p <- ggplot(data=dat, aes(x=Time, y=Value, colour = variable)) + geom_line() + theme_minimal()
      incProgress(1, detail = "Done")
      })
      
      if(input$show.peaks){
        if(!is.na(datMax$variable)){
        p <- p +
        geom_point(data=datMax, aes(x=Time, y=Value, color=variable)) +
        geom_point(data=datMax, aes(x=start.Time, y=intensity.start, label="Peak Start"),  color="red") +
        geom_point(data=datMax, aes(x=stop.Time, y=intensity.stop), label="Peak Stop", color="blue") + labs(x = "Time [s]", y = "dF/F", color = "Trace")
        }
      }
 
      
      if(input$checkFacetgrid){
        p <- p + facet_grid(variable~., scales="free")
      }
      
      return(p)
      
    }
    
  })
  
  jqui_resizable(ui="#traces")
  
  output$traces <- renderPlotly({
    
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
    
    withProgress(message = "Extracting Peaks..", value = 0, {
      
    n <- length(names(tables$tabs))
    inc <- 1

    maxTables <- lapply(seq_along(tables$tabs), FUN=function(i){
      x <- names(tables$tabs)[[i]]
      dat <- tables$tabs[[x]]
      dat$t <- row.names(dat)
      
      incProgress(inc/n, detail = paste("Analyze Table ", x, sep="\n"))
      inc <- inc + i
      x=peakAnalysis(x, dat, input, show.plots=F)
    })
    
    inc <- inc + 1
    incProgress(inc/n, detail = "Aggregating Tables..")
    maxTables <- bind_rows(maxTables, .id = "column_label")
    print(maxTables)
    
    colnames(maxTables) <- c("Table.ID", "Cell.Trace", "Peak.Time[Frame]", "Peak.Intensity[df/F]", "Table", "Peak.Start[Frame]", "Peak.Stop[Frame]", "Start.Intensity[df/F]", "Stop.Intensity[df/F]", 
                             "Peak.Duration[s]", "Duration.To.Peak[s]", "Time.To.Decay[s]", "Baseline.Intensity[df/F]", "Amplitude", "Peak.Time[s]", "Peak.Start[s]", "Peak.Stop[s]")
    #print(maxTables)
    tables$peakTables <- maxTables
    
    cols <- c("Table.ID", "Cell.Trace", "Peak.Intensity[df/F]", "Table", "Start.Intensity[df/F]", "Stop.Intensity[df/F]", "Peak.Duration[s]", "Duration.To.Peak[s]", "Time.To.Decay[s]", "Baseline.Intensity[df/F]", "Amplitude")
    
    meanTables <- aggregate(maxTables[,cols], by=list(Table.ID = maxTables$Table.ID, 
                                                      Cell.Trace = maxTables$Cell.Trace, 
                                                      Table=maxTables$Table), 
                                   FUN=function(x){
                                     if(is.numeric(x)){
                                       mean(x, na.rm=T)
                                     }
                                   })
    tables$meanTables <-  meanTables[, cols]
    incProgress(n, detail = "Done")
    
    })
    #print(tables$meanTables)
  })
  

  output$results <- DT::renderDataTable({
    if(!is.null(tables[[input$resultsTable]])){
      #tab <- tables$tabs[[input$preview_sample]]
      
      tab <- tables[[input$resultsTable]]  
      
      tab <- tab %>%  mutate_if(is.numeric, round, digits=3)
      
      DT::datatable(tab, options = list(pageLength = 20)) 
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
      #paste(input$download_table, ' Edited Table.csv', sep='')
      paste(input$tabTitle, ".csv", sep="")
    },
    content = function(file) {
      write.csv(tables[[input$download_table]], file, row.names = FALSE)
    }
  )
  
  scriptFile <- read_file("Extract_Spikes_v3.py")
  
  output$downloadPlugin <- downloadHandler(
    filename = function() {
      #paste(input$download_table, ' Edited Table.csv', sep='')
      #paste(input$tabTitle, ".csv", sep="")
      "Extract_Spikes_v3.py"
    },
    content = function(file) {
      write_file(scriptFile, file)
    }
  )
  
  
}
#}
  