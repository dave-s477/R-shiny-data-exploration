motionScorePlotUI <- function(id){
  ns <- NS(id)
  
  fluidPage(
    
    fluidRow(
      class = 'controlElements bottomLevelControl',
      h4("Plotting Options:"),
      column(10,
             uiOutput(ns("selectPlotting"))
      ),
      column(2,
             actionButton(ns("plotIgn"), "Plot Data", icon = icon("line-chart"))
      )
    ),
    fluidRow(
      uiOutput(ns("problem")),
      plotOutput(ns("plotOut"))
    )
  )
}

motionScorePlot <- function(input, output, session, data){
  ns <- session$ns
  
  motionPlot.pdf <- reactiveValues(plot = NULL)
  
  output$selectPlotting <- renderUI({
    posAggregation <- tail(names(data()), n=5)
    if(length(posAggregation) < 1){
      helpText("Filter was not yet applied, or selected data is empty.")
    } else {
      tagList(
        column(4,
              tags$div(align = 'left', 
                        class = 'multicol1', 
                        checkboxGroupInput(ns("aggregation"), "Aggregate by:", posAggregation, selected = posAggregation[1],
                                          inline   = FALSE))
        ),
        column(4,
              radioButtons(ns("scale"), "Scale:", c("Absolut" = FALSE, "Relativ" = TRUE))
              ),
        column(4,
              sliderInput(ns("sample"), "Sampling fraction", min = 0, max = 1, value = 0.5, step = 0.01)
              )
      )
    }
  })
  
  observeEvent(input$plotIgn, {
    if (!is.null(data())){
      motionPlot.pdf$plot <- plot.aggregate.ams(data(), aggregate.by = input$aggregation, relative.scale = input$scale, sample = input$sample)
      if(is.null(motionPlot.pdf$plot)){
        output$problem <- renderUI({
          helpText("It is to little data sampled to create a meaningful plot.")
        })
      } else {
        output$problem <- renderUI({
          NULL
        })
      }
      output$plotOut <- renderPlot({
        motionPlot.pdf$plot
      })
    }
  })
  
  rPlot <- reactive({
    mPlot <- motionPlot.pdf$plot
    list(plot = mPlot, height = 4)
  })
  
  return(rPlot)
}

plot.aggregate.ams <- function(ams, aggregate.by='NAME', relative.scale=FALSE, sample=.1){
  # ten second smoothing
  
  stopifnot(all(aggregate.by %in% names(ams)))
  ams$time <- as.POSIXct( (round(as.double(ams$START)/1)*1) %% (3600*24), origin=as.POSIXct('1970-01-01 01:00:00'))
  
  l <- split(ams,ams[,aggregate.by])
  l <- l[sapply(l, nrow) > 0]
  
  if(min(unlist(lapply(l, function(df){nrow(df)}))) < 10){
    return(NULL)
  } 
  
  sm <- do.call("rbind", mclapply(l, function(dd){
    gmeans <- gam(dd$VALUE ~ s(as.numeric(dd$time),bs="cs"))
    bla <- supsmu(dd$time,gmeans$fitted.values)
    baz <- supsmu(dd$time,sqrt(gmeans$residuals^2))
    rownames(dd) <- NULL
    df <- data.frame(smo=bla, sd=baz, dd[1,aggregate.by])
    if (length(aggregate.by)==1){
      names(df)[ncol(df)] <- aggregate.by
    }
    df
  })) 
  
  sm %<>%
    dplyr::sample_frac(sample)
  
  
  ams %>%
    dplyr::sample_frac(sample) %>%
    ggplot(.) + 
    #geom_point(aes(x=START, y=VALUE), alpha=.7, color='grey75') +
    geom_line(aes(x=time, y=VALUE), alpha=.7, color='grey75') +
    geom_line(data=sm, aes(x=smo.x, y=smo.y),color='orange') +
    scale_x_datetime(date_labels= "%H:%M") +
    geom_line(data=sm, aes(x=smo.x, y=pmax(0,smo.y-sd.y)),color='orange') +
    geom_line(data=sm, aes(x=smo.x, y=smo.y+sd.y),color='orange') +
    geom_ribbon(data=sm, aes(x=smo.x, ymin=pmax(0,smo.y-sd.y),ymax=smo.y+sd.y), fill='orange', alpha=.5) ->
    p
  
  if (relative.scale){
    p<- p + facet_wrap(as.formula(paste0('~', paste0(aggregate.by, collapse='+'))), scales='free_y')
  } else{
    p<- p + facet_wrap(as.formula(paste0('~', paste0(aggregate.by, collapse='+'))))
  }
  p
}
