motionScoreFilterUI <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      class = 'controlElements middleLevelControl',
      h4("Data filtering options:"),
      column(10,
             column(2,
                    uiOutput(ns("patientSelector"))
             ),
             uiOutput(ns("selectFiltering")),
             column(2,
                    uiOutput(ns("weekdaySelect"))
             )
      ),
      column(2,
             actionButton(ns("plotIgn"), "Filter Data", icon = icon("line-chart"))
      )
    ),
    fluidRow(
      verbatimTextOutput(ns("debug")),
      uiOutput(ns('plottingUI')),
      tags$div(id = ns('dynamicUI'))
    )
  )
}

motionScoreFilter <- function(input, output, session, data, patients, time){
  ns <- session$ns
  
  wd <- reactive({
    as.vector(unique(data$weekday))
  })
  
  patient_options <- reactive({
    patients
  })
  
  patientSelector(input, output, ns, patient_options, inputname = "patients", outputname = "patientSelector")
  patientSelectorUI(output, ns, patient_options, patient_options, inputname = "patients", outputname = "patientSelector")
  
  patientSelector(input, output, ns, wd, inputname = "weekday", outputname = "weekdaySelect", header = "Weekdays:")
  patientSelectorUI(output, ns, wd, wd, inputname = "weekday", outputname = "weekdaySelect", header = "Weekdays:")
  
  output$selectFiltering <- renderUI({
    daysToSelect <- unique(data$day)
    weeksToSelect <- unique(data$week)
    dmin <- min(daysToSelect)
    dmax <- max(daysToSelect)
    wmin <- min(weeksToSelect)
    wmax <- max(weeksToSelect)
    loc <- unique(data$location)
    
    tagList(
      # column(2,
      #        tags$div(align = 'left', 
      #                 class = 'multicol3', 
      #                 checkboxGroupInput(ns("patients"), "Patients:", patients, selected = patients,
      #                                    inline   = FALSE))
      #        ),
      column(2,
             tags$div(align = 'left', 
                      class = 'multicol3', 
                      checkboxGroupInput(ns("location"), "Location:", loc, selected = loc,
                                         inline   = FALSE))
             ),
      column(2,
             sliderInput(ns("days"), "Days", min = dmin, max = dmax, value = c(dmin, dmax))
             ),
      column(2,
             sliderInput(ns("weeks"), "Weeks", min = wmin, max = wmax, value = c(wmin, wmax))
      ),
      column(2,
             sliderInput(ns("time"), "Time:", min = as.integer(time[1]), max= as.integer(time[2]), value = as.integer(time), step = 1)
             )
    )
  })
  
  filtered.dataM <- reactiveValues(passToModule = NULL)
  
  output$plottingUI <- renderUI({
    motionScorePlotUI(ns("mainPlot"))
  })
  
  motionFilter.pdf <- callModule(motionScorePlot, "mainPlot", reactive({filtered.dataM$passToModule}))
  
  observeEvent(input$plotIgn, {
    patients <- input$patients
    location <- input$location
    weekdays <- input$weekday
    days <- input$days
    weeks <- input$weeks
    time <- input$time
    
    filtered.dataM$passToModule <- filter.ams(data, patients = patients, days = days[1]:days[2], weeks = weeks[1]:weeks[2], weekdays = weekdays, time = time[1]:time[2], locations = location)
  })
  
  return(motionFilter.pdf)
}

filter.ams <- function(ams, patients=NULL, days=NULL, weeks=NULL, weekdays=NULL, time=NULL, locations=NULL){
  if (is.null(patients) || is.null(days) || is.null(weekdays) || is.null(time) ||is.null(weeks) || is.null(location))
    return(NULL)
  
  if (!is.null(patients)){
    ams %<>%
      dplyr::filter(NAME %in% patients)  
  }
  if (!is.null(days)){
    ams %<>%
      dplyr::filter(day %in% days)
  }
  
  if (!is.null(weekdays)){
    ams %<>%
      dplyr::filter(weekday %in% weekdays)
  }
  
  if (!is.null(time)){
    ams %<>%
      dplyr::filter(hour(START) %in% time)
  }
  
  if(!is.null(weeks)){
    ams %<>% 
      dplyr::filter(week %in% weeks)
  }
  
  if (!is.null(location)){
    ams %<>% 
      dplyr::filter(location %in% locations)
  }
  if(nrow(ams) < 1)
    return(NULL)
  ams
}
