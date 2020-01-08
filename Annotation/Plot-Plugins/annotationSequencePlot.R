annotationSequencePlotUI <- function(id){
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      uiOutput(ns("plotWindow")),
      tags$div(id = ns("placeholder")),
      tags$div(id = ns("loadAnimation"),
               conditionalPanel(
                 condition="$('html').hasClass('shiny-busy')", tags$span(class="loader",tags$span(class="loader-inner"))
               )
               ),
      uiOutput(ns("annotationSequenceWindowZoom"))
    ),
    fluidRow(
      class = 'controlElements bottomLevelControl',
      column(2,
             uiOutput(ns('patientOptionCheckBoxes'))
      ),
      column(2,
             uiOutput(ns("annotation"))
      ),
      column(2,
             uiOutput(ns("annotater"))
      ),
      column(2,
             uiOutput(ns("time"))
             ),
      column(2,
             numericInput(ns("plotWidth"), "Plot height:", value = isolate({gQV(session, 'plotWidth',40)}), min = 20, max = 150),
             actionButton(ns("fixZoomIgn"), "Fix Zoom", class="pdfSave"),
             actionButton(ns("removeZoomIgn"), "Remove Zoom", class="pdfSave")
      ),
      uiOutput(ns('optional'))
    ),
    fluidRow(
      uiOutput(ns("interraterReliability"))
    )
  )
}

annotationSequencePlot <- function(input, output, session, annotation, colors, categories, type = "patient"){
  ns <- session$ns
  
  patient_options <- reactive({
    as.vector(unique(annotation$NAME))
  })
  
  annotation_options <- reactive({
    as.vector(as.character(unique(annotation$CATEGORY)))
  })
  
  annotater_options <- reactive({
    as.character(unique(annotation$OBSERVER))
  })
  
  patientSelector(input, output, ns, patient_options)
  patientSelectorUI(output, ns, patient_options, patient_options)
  
  patientSelector(input, output, ns, annotation_options, inputname = "selectedAnnotation", outputname = "annotation", header = "Select Annotation:")
  patientSelectorUI(output, ns, annotation_options, annotation_options, inputname = "selectedAnnotation", outputname = "annotation", header = "Select Annotation:")
  
  patientSelector(input, output, ns, annotater_options, inputname = "selectedAnnotater", outputname = "annotater", header = "Select Observer:")
  patientSelectorUI(output, ns, annotater_options, annotater_options, inputname = "selectedAnnotater", outputname = "annotater", header = "Select Observer:")
  
  output$time <- renderUI({
    start <- head(annotation$START, n=1)
    end <- tail(annotation$END, n=1)
    days <- ceiling(as.numeric(end - start))
    sliderInput(ns('timeFilter'), 'Choose Day Filter: ', min = 1, max = days, value = isolate({gQV(session, 'timeFilter',c(1, days))}))
  })
  
  output$optional <- renderUI({
    if (type == "observer"){
      column(2,
             radioButtons(ns("double"), "Show double Annotations:", c("Inclusively" = "inc", "Exclusively" = "ex")),
             conditionalPanel(
               sprintf("input['%s'] == 'ex'", ns("double")),
               helpText("To compare double Annotations, the Interrater Reliability (IRR) can be calculated on the main selected data. 
                        Note: one observer might select multiple categories per patient per time "),
               radioButtons(ns("irr"), "Calculate IRR", c("calculate" = "calc", "ignore" = "ig"), selected = "ig")
               ),
             actionButton(ns("reloadIgn"), "Reload Plot", icon = icon("refresh"))
      )
    } else {
      column(2,
             helpText("No further selection possible."),
             actionButton(ns("reloadIgn"), "Reload Plot", icon = icon("refresh"))
             )
    }
  })
  
  dataIsDoubleFiltered <- reactiveValues(bool = FALSE)
  
  output$interraterReliability <- renderUI({
    
    if (type == "observer" && input$double == "ex" && length(input$irr) > 0 && input$irr == "calc") {
      tagList(
        h3("Interrater Reliability (IRR)"),
        helpText(
          "The IRR is examining time slots, in which multiple observers annotated the behaviour of the same Patient.
          The IRR calculates how often the annotations of multiple observers matched. Behaviour is annotated in 5 minute intervals.
          Note that one observer might annotate multiple behaviours in an interval.
          Reloading the plot triggers the calculation on the current data."
        ),
        HTML(
          calculateIRR(mainVals$filteredAnnotation,
                       type,
                       dataIsDoubleFiltered$bool)
        ),
        br(),
        br()
        )
    } else {
      NULL
    }
  })
  
  mainVals <- reactiveValues(
    filteredAnnotation = annotation, 
    plotHeight = ((length(unique(annotation$NAME)) * 40) + 90),
    mainPlot = NULL)
  
  observeEvent(input$reloadIgn, {
    if (!is.null(input$reloadIgn)) {
      if (type == "patient") {
        mainVals$filteredAnnotation <-
          filterAnnotationSequenceAll(
            annotation,
            input$selectedPatients,
            input$selectedAnnotation,
            input$selectedAnnotater,
            input$timeFilter
          )
      } else if (type == "observer") {
        mainVals$filteredAnnotation <-
          filterAnnotationSequenceAll(
            annotation,
            input$selectedPatients,
            input$selectedAnnotation,
            input$selectedAnnotater,
            input$timeFilter,
            input$double
          )
      }
      mainVals$plotHeight <-
        (length(unique(mainVals$filteredAnnotation$NAME)) * input$plotWidth) + 90
    }
    output$plotWindow <- renderUI({
      plotOutput(
        ns("plot"),
        height = paste0(mainVals$plotHeight, "px"),
        brush = brushOpts(id = ns("plot_brushIgn"),
                          resetOnNew = TRUE)
      )
    })
    
    if (type == "patient") {
      mainVals$mainPlot <-
        dmzAnnotationTimeAllPlot.patient(mainVals$filteredAnnotation, colors, categories)
    } else if (type == "observer") {
      mainVals$mainPlot <-
        dmzAnnotationTimeAllPlot.observer(mainVals$filteredAnnotation, colors, categories)
    }
    
    output$plot <- renderPlot({
      mainVals$mainPlot
    })
    
    if(length(input$double) > 0) {
      if (input$double == "ex") {
        dataIsDoubleFiltered$bool <- TRUE
      } else if (input$double == "inc") {
        dataIsDoubleFiltered$bool <- FALSE
      }
    }
  },
  ignoreNULL = FALSE)
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  observe({
    brush <- input$plot_brushIgn
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  zoomPlotHeight <- reactive({
    if(type == "patient"){
      (length(unique(zoomFilteredAnnotationSequence()$NAME)) * input$plotWidth) + 80
    } else if (type == "observer") {
      (length(unique(zoomFilteredAnnotationSequence()$OBSERVER)) * input$plotWidth * 3) + 80
    }
  })
  
  output$annotationSequenceWindowZoom <- renderUI({
    if(is.null(ranges$x) | is.null(ranges$y))
      return(NULL)
    plotOutput(ns("annotationSequenceZoom"),
               height = paste0(zoomPlotHeight(), "px"))
  })
  
  zoomFilteredAnnotationSequence <- reactive({
    if(type == "patient"){
      remainingNames <- unique(mainVals$filteredAnnotation$NAME)
      lvls <- levels(mainVals$filteredAnnotation$NAME)
      rl <- intersect(lvls, remainingNames)
      names <- rl[round(ranges$y[1]):(round(ranges$y[2])-1)]
      range <- c(as.POSIXct(ranges$x[1], origin = "1970-01-01"), as.POSIXct(ranges$x[2], origin = "1970-01-01"))
      range <- testRanges(range)
      mainVals$filteredAnnotation %>%
        filter(NAME %in% names & START >= range[1] & START <= range[2]) ->
        fData
      fData
    } else if (type == "observer") {
      remainingNames <- unique(mainVals$filteredAnnotation$OBSERVER)
      lvls <- levels(mainVals$filteredAnnotation$OBSERVER)
      rl <- intersect(lvls, remainingNames)
      names <- rl[round(ranges$y[1]):(round(ranges$y[2])-1)]
      range <- c(as.POSIXct(ranges$x[1], origin = "1970-01-01"), as.POSIXct(ranges$x[2], origin = "1970-01-01"))
      range <- testRanges(range)
      mainVals$filteredAnnotation %>%
        filter(OBSERVER %in% names & START >= range[1] & START <= range[2]) ->
        fData
      fData
    }
  })
  
  plotO <- reactive({
    if(type == "patient"){
      dmzAnnotationTimeAllPlot.patient(zoomFilteredAnnotationSequence(), colors, categories)
    } else if (type == "observer") {
      dmzAnnotationTimeAllPlot.observer(zoomFilteredAnnotationSequence(), colors, categories)
    }
  })
  
  output$annotationSequenceZoom <- renderPlot({
    plotO()
  })
  
  zoomList <- reactiveValues(index = 0, display = list(), outValues = list())
  
  observeEvent(input$fixZoomIgn,
               {
                 brush <- input$plot_brushIgn
                 if(!is.null(brush)){
                   if(type == "patient"){
                     height <- (length(unique(zoomFilteredAnnotationSequence()$NAME)))+0.5
                   } else if (type == "observer") {
                     height <- (length(unique(zoomFilteredAnnotationSequence()$OBSERVER)) * 2.5)+ 0.5
                   }
                   annotaters <- input$selectedAnnotater
                   iniRanges <- testRanges(c(as.POSIXct(ranges$x[1], origin = "1970-01-01"), as.POSIXct(ranges$x[2], origin = "1970-01-01")))
                   zoomList$index <- zoomList$index + 1 
                   currentIndex <- zoomList$index
                   zoomList$display[[currentIndex]] = FALSE
                   
                   zoomList[[as.character(currentIndex)]] <- list(x = c(brush$xmin, brush$xmax), y = c(brush$ymin, brush$ymax))
                   
                   insertUI(
                     selector = paste0('#', ns('placeholder')),
                     ui = tags$div(
                       plotOutput(ns(paste0("annotationSequenceZoom",currentIndex)),
                                  height = paste0(zoomPlotHeight(), "px"),
                                  click = ns(paste0("re_clickIgn",currentIndex)),
                                  dblclick = ns(paste0("re_zoom_clickIgn",currentIndex)),
                                  brush = brushOpts(
                                    id = ns(paste0("re_zoom_brushIgn",currentIndex)),
                                    resetOnNew = TRUE
                                  )),
                       uiOutput(ns(paste0('tooltip',currentIndex))),
                       id = ns(paste0("zoom", currentIndex))
                     )
                   )
                   
                   ranges[[paste0('x',currentIndex)]] <- NULL
                   ranges[[paste0('y',currentIndex)]] <- NULL
                   
                   observeEvent(input[[paste0('re_zoom_clickIgn', currentIndex)]],
                                {
                                  brush <- input[[paste0('re_zoom_brushIgn',currentIndex)]]
                                  if (!is.null(brush)) {
                                    ranges[[paste0('x',currentIndex)]] <- testRanges(c(as.POSIXct(brush$xmin, origin = "1970-01-01"), as.POSIXct(brush$xmax, origin = "1970-01-01")))
                                    ranges[[paste0('y',currentIndex)]] <- c(round(brush$ymin), round(brush$ymax))
                                    
                                  } else {
                                    ranges[[paste0('x',currentIndex)]] <- NULL
                                    ranges[[paste0('y',currentIndex)]] <- NULL
                                  }
                                })
                   
                   observeEvent(input[[paste0("re_clickIgn", currentIndex)]], {
                     if(zoomList$display[[currentIndex]]){
                       output[[paste0("tooltip", currentIndex)]] <- renderUI({
                         NULL
                       })
                       zoomList$display[[currentIndex]] <- FALSE
                     } else {
                       output[[paste0("tooltip", currentIndex)]] <- renderUI({
                         verbatimTextOutput(ns(paste0("zoomWindow", currentIndex)))
                       })
                       
                       output[[paste0("zoomWindow", currentIndex)]] <-
                         renderPrint({
                           if(is.null(ranges[[paste0('x', currentIndex)]])){
                             paste0("Time displayed: ",paste(iniRanges, collapse = " - "), ", Annotaters selected:", paste(annotaters, collapse = ", "))
                           } else {
                             paste0("Time displayed: ",paste(ranges[[paste0('x',currentIndex)]], collapse = " - "), ", Annotaters selected:", paste(annotaters, collapse = ", "))
                           }
                         })
                       zoomList$display[[currentIndex]] <- TRUE
                     }
                   })
                   
                   p <- plotO()
                   
                   sPlot <- reactive({
                     p + 
                       coord_cartesian(xlim = ranges[[paste0('x',currentIndex)]])#, ylim = ranges[[paste0('y', zoomList$index)]])
                   })
                   
                   output[[paste0('annotationSequenceZoom',currentIndex)]] <- renderPlot({
                     sPlot()
                   })
                   
                   patients <- input$selectedPatients
                   annotation <- input$selectedAnnotation
                   zoomList$outValues[[currentIndex]] <- list(patients = patients, annotation = annotation, annotater = annotaters, reactive_ranges = reactive(ifelse(is.null(ranges[[paste0('x', currentIndex)]]), paste0(iniRanges, collapse = ' to '), paste0(ranges[[paste0('x',currentIndex)]], collapse=' to '))), plot = sPlot, height = height)
                 }
               })
  
  observeEvent(input$removeZoomIgn,
               {
                 removeUI(
                   selector = paste0('#', ns(paste0('zoom',zoomList$index)))
                 )
                 if(zoomList$index > 0){
                   zoomList[[as.character(zoomList$index)]] <- NULL
                   zoomList$outValues[[zoomList$index]] <- NULL
                   zoomList$index <- zoomList$index - 1
                 }
               })
  
  rlist <- reactive({
    mPlot <- mainVals$mainPlot
    height <- ifelse(type == 'patient', length(input$selectedPatients)/2+1, length(input$selectedAnnotater)/2+1)
    list(plot = mPlot, height = height, additionalBaggage = zoomList$outValues)
  })
  
  return(rlist)
}

dmzAnnotationTimeAllPlot.patient <- function(plot_data, colorScheme, categories){
  
  plot_data %>%
    group_by(START, NAME) %>%
    arrange(CATEGORY) %>%
    mutate(i=row_number(), n=n()) ->
    df.rank
  
  size <- 0
  
  p <- ggplot(df.rank, aes(START, as.numeric(factor(NAME)))) + 
    geom_rect(size=size,aes(fill=CATEGORY, xmin=START, xmax=START + minutes(5), ymin=((i-1)/n) + as.numeric(factor(NAME)), ymax=(i/n) + as.numeric(factor(NAME)))) + 
    scale_y_continuous('Patient', breaks=as.numeric(unique(factor(df.rank$NAME)))+.5, labels=unique(factor(df.rank$NAME))) +
    #scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:00") + 
    scale_fill_manual(values = colorScheme) +
    theme(legend.position = 'top') +
    guides(fill=guide_legend(nrow=1,byrow=TRUE))
  return(p)
}

dmzAnnotationTimeAllPlot.observer <- function(plot_data, colorScheme, categories){
  plot_data %>%
    group_by(START, OBSERVER) %>%
    arrange(CATEGORY) %>%
    mutate(i=row_number(), n=n()) ->
    df.rank
  
  size <- 0
  
  ggplot(df.rank, aes(START, as.numeric(factor(OBSERVER)))) + 
    geom_rect(size=size, aes(fill=CATEGORY, xmin=START, xmax=START + minutes(5), ymin=((i-1)/n) + as.numeric(factor(OBSERVER)), ymax=(i/n) + as.numeric(factor(OBSERVER)))) + 
    scale_y_continuous('OBSERVER', breaks=as.numeric(unique(factor(df.rank$OBSERVER)))+.5, labels=unique(factor(df.rank$OBSERVER))) +
    #scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:00") + 
    scale_fill_manual(values = colorScheme) +
    theme(legend.position = 'top') +
    guides(fill=guide_legend(nrow=1,byrow=TRUE))
}

filterAnnotationSequenceAll <- function(data, patients, annotation_sel, annotater_sel, time_sel, multiAnno = 'inc'){
  start_data <- ymd_hms(head(data$START, n=1))
  start_mark <- as.Date(start_data + days(time_sel[1]-1))
  end_mark <- as.Date(start_data + days(time_sel[2]))
  data %>%
    filter(NAME %in% patients & CATEGORY %in% annotation_sel & OBSERVER %in% annotater_sel & START >= start_mark & START <= end_mark) ->
    filteredData
  
  if(multiAnno == 'ex'){
    filteredData %>% 
      group_by(NAME, START) %>% 
      filter(n()>1 & n()<3) %>% 
      group_by(NAME, START, OBSERVER) %>% 
      filter(n()<2) -> 
      filteredData
  }
  return(filteredData)
}

testRanges <- function(ranges){
  fixedRanges <- c()
  if(hour(ranges[1]) < 9 | hour(ranges[1]) > 18){
    if(hour(ranges[1]) > 18){
      fixedRanges[1] <- ceiling_date(ranges[1], "day") + hours(9)
    } else {
      fixedRanges[1] <- floor_date(ranges[1], "day") + hours(9)
    }
  } else {
    fixedRanges[1] <- ranges[1]
  }
  if(hour(ranges[2]) < 9 | hour(ranges[2]) > 18){
    if(hour(ranges[2]) > 18){
      fixedRanges[2] <- floor_date(ranges[2], "day") + hours(19)
    } else {
      fixedRanges[2] <- floor_date(ranges[2], "day") - days(1) + hours(19)
    }
  } else {
    fixedRanges[2] <- ranges[2]
  }
  return(as.POSIXct(fixedRanges, origin = "1970-01-01"))
}

calculateIRR <- function(data, type, dataFiltered){
  if(!dataFiltered){
    return("Reload the plot with exclusive multiple annotation data to calculate corresponding IRR.")
  } else {
    if(nrow(data) < 1){
      return("<strong>Selected data is empty</strong>")
    }
    data %>%
      group_by(NAME, START) %>%
      arrange(OBSERVER) %>%
      filter(2==n_distinct(OBSERVER)) %>%
      mutate(ob =ifelse(OBSERVER==unique(OBSERVER)[1], 'ob1', 'ob2')) %>%
      dcast(., START+NAME~ob+CATEGORY, fun.aggregate = length, drop=FALSE) ->
      twice
    
    rAM <- select(twice, starts_with('ob1'))
    rBM <- select(twice, starts_with('ob2'))
    
    irr <- round(mkappa(rAM, rBM), 3)
    
    return(paste0("<strong>Calculated Result: </strong>", " IRR = ", irr))
  }
}

mkappa <- function(rAM, rBM){
  k <- ncol(rAM)
  n <- nrow(rAM)
  xi <- rowSums(rAM==1 & rBM==1)
  Ai <- rowSums(rAM==1 & rBM==0) + xi
  Bi <- rowSums(rAM==0 & rBM==1) + xi
  
  pi <- 1 - Ai/k  - Bi/k  + 2*xi/k
  pi <- sum(pi)/n
  
  pi0 <- sum(1-Ai/k - Bi/k + 2*Ai*Bi/(k*k))/n
  
  kappa<- (pi - pi0)/(1 - pi0)
  kappa
}
