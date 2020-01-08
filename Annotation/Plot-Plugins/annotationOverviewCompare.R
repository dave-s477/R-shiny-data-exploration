# Setting up the UI for the nested module that allows a compare view of annotation overviews.
annotationOverviewCompareUI <- function(id){
  # Setting up the distinc namespace 
  ## The namespace is nested, so it already contains the namespace of the parent module
  ns <- NS(id)
  
  # Again return the UI as a tagList
  tagList(
    column(6,
           fluidRow(
             class = 'controlElements middleLevelControl',
             fluidRow(
               h4(paste0("Control Options:"))
             ),
             fluidRow(
               column(6,
                      uiOutput(ns('patientOptionCheckBoxes'))
               ),
               column(6,
                      numericInput(ns("plotWidth"), "Adjust plot height (pixel per bar):", value = isolate({gQV(session, 'plotWidth',40)}), min = 20, max = 150),
                      radioButtons(ns("possibleTimeRaster"), "Time steps:",
                                   c("Weeks" = "week",
                                     "Days" = "day",
                                     "Hours" = "hour"), 
                                   selected = isolate({gQV(session, 'possibleTimeRaster', "week")})),
                      uiOutput(ns('dateReachDmz')),
                      actionButton(ns("refreshCompareIgn"), paste0("Reload Compare"), icon = icon("refresh"), style='background: lightblue'),
                      br()
               )
             )
           ), 
           fluidRow(
             uiOutput(ns("dmzCompare"))
           )
    )
  )
}

# Server logic of the nested compare module 
annotationOverviewCompare <- function(input, output, session, annotation, colors, dmzDataOptions, sortingOrder, sortdesc, categorieS, type = 'DZNE'){
  ns <- session$ns
  
  patient_options <- reactive({
    as.vector(unique(annotation()$NAME))
  })
  
  # The following construct is used to create a custom UI element that contains a checkbox group + select/unselect all buttons
  patientSelector(input, output, ns, patient_options)
  patientSelectorUI(output, ns, patient_options, patient_options)
  
  queriedTimeDuration <- reactive({
    getDurationQuery(dmzDataOptions())
  })
  
  dmzTimeDurations <- reactive({
    dmzTimeDurationCalculation(queriedTimeDuration(), input$possibleTimeRaster)
  })
  
  # Dynamic UI selection options are setup based on the available data
  output$dateReachDmz <- renderUI({
    if(length(dmzDataOptions()) == 0)
      return(helpText("Aktuell sind keine Daten ausgewählt"))
    
    if(input$possibleTimeRaster == "week"){
      sliderInput(ns("comparedTime"), paste0("Choose weeks to compare (", paste0(dmzDataOptions(), collapse=", "),"):"),  
                  min = 1, max = dmzTimeDurations()[[1]], value = isolate({gQV(session, "comparedTime", c(1,dmzTimeDurations()[[1]]))}))
    } else if (input$possibleTimeRaster == "day") {
      sliderInput(ns("comparedTime"), paste0("Choose days to compare (", paste0(dmzDataOptions(), collapse=", "),"):"),  
                  min = 1, max = dmzTimeDurations()[[1]], value = isolate({gQV(session, "comparedTime", c(1,dmzTimeDurations()[[1]]))}))
    } else {
      tagList(
        sliderInput(ns("comparedTime"), paste0("Choose days to compare (", paste0(dmzDataOptions(), collapse=", "),"):"),  
                    min = 1, max = dmzTimeDurations()[[1]], value = isolate({gQV(session, "comparedTime", c(1,dmzTimeDurations()[[1]]))})),
        sliderInput(ns("comparedTimeHourAddition"), paste0("Choose hours to compare (", paste0(dmzDataOptions(), collapse=", "),"):"),  
                    min = dmzTimeDurations()[[2]][1], max = dmzTimeDurations()[[2]][2], value = isolate({gQV(session, "comparedTime", dmzTimeDurations()[[2]])}))
      )
    }
  })
  
  plotHeight <- eventReactive(input$refreshCompareIgn, {
    (length(input$selectedPatients) * input$plotWidth) + 70
  })
  
  # Reactive Values are a list of values that can trigger reactions
  # NOTE: reactiveValues are handled as a list of values, where each value change can trigger a reaction. 
  # The list itself DOES NOT trigger reactions.
  outPlot <- reactiveValues(plot = 0)
  
  # ObserveEvent constructs can be used if reactions should be triggered by observing on specific reactive element
  ## In this case a click on the button, that is supposed to refresh the compare view with the current selected values
  # Everything inside the statement is handled in an isolate scope and just executed if the trigger is observed.
  # -> Other reactives are ignored. (NOTE: newly created reactives in the scope can undermine the concept)
  # "observeEvent" ignores return Values, if a values needs to be returned use "eventReactive"
  observeEvent(input$refreshCompareIgn, {
    isolate({
      if (length(dmzDataOptions()) == 0 |
          length(input$selectedPatients) == 0) {
        output$dmzCompare <- renderUI({
          helpText("There is nothing to plot.")
        })
      } else {
        output$dmzCompare <- renderUI({
          plotOutput(ns("dmzCompareRendered"), height = paste0(plotHeight(), "px"))
        })
        
        compareData <-
          dmzDataTransform(
            dmzCompareQuery(
              dmzDataOptions(),
              input$selectedPatients,
              queriedTimeDuration(),
              input$possibleTimeRaster,
              input$comparedTime,
              input$comparedTimeHourAddition,
              type
            )
          )
        
        if(is.null(compareData)){
          output$dmzCompare <- renderUI({
            helpText("No data for the current selection.")
          })
        } else {
          compareData <- restructureDmzData(compareData, sortingOrder(), sortdesc(), type)
          
          outPlot$plot <- dmzAveragePlot(compareData, colors, categorieS)
          
          output$dmzCompareRendered <- renderPlot({
            outPlot$plot
          })
        }
      }
    })
  })
  
  # Setting up the return value of this module
  ## Returning reactive elements causes more predictable behaviour
  rPlot <- reactive({
    list(plot = outPlot$plot, height = length(input$selectedPatients)/2+1)
  })
  
  # The return statment is reactivly updated
  return(rPlot)
}

# Get the required data for the compare from the database
dmzCompareQuery <- function(dbName, patients, dbTimes, timeRaster, chosenTimeSlot, hours, type){
  compareData <- NULL
  patientClause <- paste0("'",patients, "'", collapse = ", ")
  for(i in dbName){
    conn <- dbConnect(MonetDB.R(), host="localhost", dbname=i, user="monetdb", password="password")
    timeSlot <- getTimeSlot(dbTimes[dbTimes$location == i,]$starts, timeRaster, chosenTimeSlot, hours)
    timeVal1 <- paste0("'", toString(timeSlot[[1]][1]), "'")
    timeVal2 <- paste0("'", toString(timeSlot[[1]][2]), "'")
    hoursTime1 <- paste0("'", timeSlot[[2]][1], ":00:00'")
    hoursTime2 <- paste0("'", timeSlot[[2]][2], ":00:00'")
    if(type == "DZNE"){
      queryPart1 <- paste0("n_count as (select \"NAME\", count(*) as a from extendedannotation inner join raw.patient rp on rp.\"ID\"=extendedannotation.\"PATIENT$ID\" where \"NAME\" in (", patientClause,") and \"START\" >= ", timeVal1, " and \"START\" <= ", timeVal2 ," and cast(\"START\" as time)>=", hoursTime1," and cast(\"START\" as time)<=", hoursTime2 , " group by \"NAME\"), ")
      queryPart2 <- paste0("nc_count as (select \"NAME\", \"CATEGORY\", count(*) as b from extendedannotation inner join raw.patient rp on rp.\"ID\"=extendedannotation.\"PATIENT$ID\" where \"NAME\" in (", patientClause,") and \"START\" >= ", timeVal1, " and \"START\" <= ", timeVal2 ," and cast(\"START\" as time)>=", hoursTime1," and cast(\"START\" as time)<=", hoursTime2 , " group by \"NAME\", \"CATEGORY\") ")
      queryPart3 <- "select nc_count.\"NAME\", \"CATEGORY\", cast(nc_count.b as real)/cast(n_count.a as real) as freq from n_count inner join nc_count on n_count.\"NAME\"=nc_count.\"NAME\";"
    } else if (type == "DCM") {
      queryPart1 <- paste0("n_count as (select \"NAME\", count(*) as a from raw.annotation inner join raw.patient rp on rp.\"ID\"=annotation.\"PATIENT$ID\" where \"TYPE\"='DCM' and \"NAME\" in (", patientClause,") and \"START\" >= ", timeVal1, " and \"START\" <= ", timeVal2 ," and cast(\"START\" as time)>=", hoursTime1," and cast(\"START\" as time)<=", hoursTime2 , " group by \"NAME\"), ")
      queryPart2 <- paste0("nc_count as (select \"NAME\", \"CATEGORY\", count(*) as b from raw.annotation inner join raw.patient rp on rp.\"ID\"=annotation.\"PATIENT$ID\" where \"TYPE\"='DCM' and \"NAME\" in (", patientClause,") and \"START\" >= ", timeVal1, " and \"START\" <= ", timeVal2 ," and cast(\"START\" as time)>=", hoursTime1," and cast(\"START\" as time)<=", hoursTime2 , " group by \"NAME\", \"CATEGORY\") ")
      queryPart3 <- "select nc_count.\"NAME\", \"CATEGORY\", cast(nc_count.b as real)/cast(n_count.a as real) as freq from n_count inner join nc_count on n_count.\"NAME\"=nc_count.\"NAME\";"
    }
    query <- paste0("with ", queryPart1, queryPart2, queryPart3)
    queriedData <- dbGetQuery(conn, query)
    dbDisconnect(conn)
    
    compareData <- rbind(queriedData, compareData)
  }
  return(compareData)
}

# Function that calculated the possible selectable time slot
getTimeSlot <- function(dbTimes, timeRaster, chosenTimeSlot, hours){
  start <- dbTimes
  if(timeRaster == "week"){
    startChoice <- start + weeks(chosenTimeSlot[1]-1)
    endChoice <- start + weeks(chosenTimeSlot[2])
    hourC <- c(0, 24)
  } else if(timeRaster == "day") {
    startChoice <- start + days(chosenTimeSlot[1]-1)
    endChoice <- start + days(chosenTimeSlot[2])
    hourC <- c(0, 24)
  } else {
    startChoice <- start + days(chosenTimeSlot[1]-1)
    endChoice <- start + days(chosenTimeSlot[2])
    hourC <- hours
  }
  return(list(c(startChoice, endChoice), hourC))
}

# To setup the UI dynamic the amount of days in a database is calculated using the following functions
dmzTimeDurationCalculation <- function(time, timeRaster){
  diff.value <- max(time$diff)
  hours <- c(0, 24)
  if(timeRaster == "week"){
    return(list(ceiling(diff.value/7), hours))
  } else if(timeRaster == "day"){
    return(list(diff.value, hours))
  } else {
    return(list(diff.value, hours))
  }
}

# Calculate the time limits in the given database
getDurationQuery <- function(databases){
  minmax <- NULL
  for(i in databases){
    conn <- dbConnect(MonetDB.R(), host="localhost", dbname=i, user="monetdb", password="password")
    queriedData <- dbGetQuery(conn, paste0("select * from location"))
    dbDisconnect(conn)
    queriedData %>% 
      group_by(location) %>%
      summarise(starts = as.Date(ymd_hms(min(START))), ends = as.Date(ymd_hms(max(END))), diff = round(as.numeric(max(ymd_hms(END))-min(ymd_hms(START))))) -> extracted.times
    
    minmax <- rbind(extracted.times, minmax)
  }
  return(minmax)
}
