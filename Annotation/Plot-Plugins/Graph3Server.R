source("Plot-Plugins/Graph3Plots.R")

############################# Globals #############################
max_days <- 0

######################################DB Query######################################
#Runs the Queryfunction for every selected Database
queryForDB <- function(database, QueryFunc){
  if(length(database) == 1) {
    data <- QueryFunc(database[1])
    return(data)
  } else {
    data <- QueryFunc(database[1])
    for(i in 2:length(database)){
      data_next <- QueryFunc(database[i])
      data <- rbind(data, data_next)
    }
    return(data)
  }
}

#Query for the Aggregation Plot
aggregationQuery <- function(dbName){
  conn <- dbConnect(MonetDB.R(), host="localhost", dbname=dbName, user="monetdb", password="password")
  queriedData <- dbGetQuery(conn, "select \"RECORDING$ID\", \"ACCEL_START\", \"ACCEL_END\", \"PROTOCOL_START\", \"PROTOCOL_END\", \"NAME\", \"START_SENSOR$ID\", \"START_LIMB\" from raw.protocoltorecording pr inner join raw.protocol p on p.\"ID\"=pr.\"PROTOCOL$ID\" inner join raw.patient pa on pa.\"ID\"=p.\"PATIENT$ID\";")
  dbDisconnect(conn)
  date_strings <- c(min(queriedData$PROTOCOL_START), max(queriedData$PROTOCOL_START))
  datetimes <- strptime(date_strings, format = "%Y-%m-%d")
  max_days <<- difftime(datetimes[2], datetimes[1], units = "days")
  return(queriedData)
}

#Get the Data for the other Plots
intervalQuery <- function(db){
  conn <- dbConnect(MonetDB.R(), host='localhost', dbname=db, username='monetdb', password='password')
  
  acc_range <- dbGetQuery(conn, 'select * from acc_range') %>%
    dplyr::mutate(NAME=factor(NAME), LIMB=factor(LIMB), START=as.POSIXct(START), END=as.POSIXct(END)) %>%
    dplyr::mutate(interval=interval(START, END))
  
  annotation_range <- dbGetQuery(conn, 'select * from annotation_range') %>%
    dplyr::mutate(NAME=factor(NAME), TYPE=factor(TYPE), START=as.POSIXct(START), END=as.POSIXct(END)) %>%
    dplyr::mutate(interval=interval(START, END))
  
  dbDisconnect(conn)
  list(acc_range=acc_range, annotation_range=annotation_range)
}

intervalQueryC <- memoise(intervalQuery)


############################# Server Tabs #############################

graph3Server <- function(input, output, session){
  dataAvailability.pdf <- list()
  
  observeEvent(input$subtabs, {
    if(input$subtabs == "aggr") {
      dataAvailability.pdf$plot3 <<- graph3ServerAggr(input, output, session)
    }
    else if(input$subtabs == "fixed") {
      dataAvailability.pdf$plot1 <<- graph3ServerFixed(input, output, session)
      #graph3ServerFixed(input, output, session)
    } 
    else {
      dataAvailability.pdf$plot2 <<- graph3ServerFloating(input, output, session)
    }
  })
  
  output$pdfSave <- downloadHandler(
    filename = "dataAvailability.pdf",
    content = function(file){
      dataAvailabilityPlots <- list(plot1 = NULL, plot2 = NULL, plot3 = NULL)
      if(length(dataAvailability.pdf) > 0){
        for(i in 1:length(dataAvailability.pdf)){
          if(!is.null(dataAvailability.pdf[[paste0("plot",i)]])){
            dataAvailabilityPlots[[paste0("plot",i)]] <- dataAvailability.pdf[[paste0("plot",i)]]()
          }
        }
      }
      params <- list(title = "Data Availability", mainPlotExists = FALSE, inputs = reactiveValuesToList(input), plots = dataAvailabilityPlots)#, explicitPattern = explicitPattern)
      rmarkdown::render("Rmds/reportGenerator.Rmd", output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
    }
  )
}

############################# Server FUNCTION #############################

#Server Function for aggragation Tab
graph3ServerAggr <- function (input, output, session) {
  ns <- session$ns
  
  serverAggr <- reactiveValues(plot = NULL)
  
  #Check if data can be printed (avoids null errors)
  checkPrint <- function (input) {
    if (!is.null(input$dmzDataSets) && !is.null(input$selectedPatients3)) {TRUE}
    else {FALSE}
  }
  
  #Plot Data as a reactive Value
  protocolData <- reactive({
    if (!is.null(input$dmzDataSets)) {
      queryForDB(input$dmzDataSets, aggregationQuery)
    }
  })
  
  patient_options <-  reactive({
    if (!is.null(input$dmzDataSets)) {
      sort(unique(protocolData()$NAME))
    }
  })
  
  patientSelector(input, output, ns, patient_options, inputname = "selectedPatients3")
  patientSelectorUI(output, ns, patient_options, patient_options, inputname = "selectedPatients3")
  
  output$daySlider <- renderUI({
    if (checkPrint(input)) {
      date_strings <- c(min(protocolData()$PROTOCOL_START), max(protocolData()$PROTOCOL_START))
      datetimes <- strptime(date_strings, format = "%Y-%m-%d")
      diff_in_days <- difftime(datetimes[2], datetimes[1], units = "days")
      tagList(
        sliderInputQ(ns, ("days3"), "Daytime of Observation:", min = 0, max = 24, value = c(13,17)),
        sliderInput(ns("aggr3"), "Number of Days to Aggregate:", min = 1, max = (as.integer(max_days)), value = isolate({gQV(session, "aggr3", 14)}), step = 1)
      )
    }
  })
  
  #output$debug0 <- renderPrint({})
  
  output$aggrPlot <- renderPlot({
    if (is.integer(input$days3) && checkPrint(input)) {
      start<-input$days3[1]
      end<-input$days3[2] 
      aggr.days<-input$aggr3
      
      protocol <- protocolData()
      protocol$overlap <- intersect(interval(protocol$ACCEL_START, protocol$ACCEL_END, tz='CEST'), interval(protocol$PROTOCOL_START, protocol$PROTOCOL_END, tz='CEST'))
      protocol$day <- as.integer(strftime(protocol$ACCEL_START, format='%j'))
      protocol$location <- ifelse(grepl(protocol$NAME, pattern = '^X1'), "Location1", "Location2")
      protocol$week <- unsplit(lapply(split(protocol, protocol$location), function(pp){
        week = floor((pp$day - min(pp$day)) / aggr.days)
      }), protocol$location)
      
      starttime <- ymd_h(paste0(lubridate::date(protocol$ACCEL_START)," ", start))
      if (end <= start){
        endtime <- ymd_h(paste0(lubridate::date(protocol$ACCEL_START)," ", end)) + days()
      }else{
        endtime <- ymd_h(paste0(lubridate::date(protocol$ACCEL_START)," ", end))
      }
      protocol$enough <- ((starttime %within% protocol$overlap) & (endtime %within% protocol$overlap))
      
      dplyr::group_by(protocol, NAME, START_LIMB, week, location) %>%
        dplyr::summarise(n=sum(enough))-> 
        stat
      
      stat <- filter(stat, NAME %in% input$selectedPatients3)
      
      stat %>% 
        group_by(START_LIMB) %>%
        summarise(min=min(n)) ->
        stat.min
      
      serverAggr$plot <- aggregationPlot(stat, stat.min, start, end)
      serverAggr$plot
    }
  })
  
  rlist <- reactive({
    mPlot <- serverAggr$plot
    list(plot = mPlot, height = 4)
  })
  
  return(rlist)
}

#Server Function for the other Plots
graph3ServerFloating <- function (input, output, session) {
  ns <- session$ns
  
  serverSingle <- reactiveValues(plot2 = NULL, plot3 = NULL)
  
  output$settings <- renderUI({
    tagList(
      checkboxInputQ(ns, ("fixedscale2"), "Fixed Scale", value = TRUE),
      sliderInputQ(ns, ("rangesteps2"), "Sampling Rate:", min = 0.1, max = 2, value = .5, step = 0.1)
    )
  })
  
  output$intervalslider <- renderUI({
    sliderInputQ(ns, ("interval2"), "Interval Boundaries:", min = 0, max = 24, value = c(13,17))
  })
  
  output$intervalLength <- renderUI({
    if (!is.null(input$interval2)) 
      sliderInputQ(ns, ("length2"), "Interval Length:", min = 0, max = (input$interval2[2] - input$interval2[1]), value = 1, step = input$rangesteps2)
  })
  
  #Oberserve any changes to the Databases, reactive variables create problems when assigning diff
  observeEvent(input$dmzDataSets2, {
    if(length(input$dmzDataSets2) == 1) {
      dfo <- intervalQueryC(input$dmzDataSets2[1])
      annotation_range <- dfo$annotation_range
      acc_range <- dfo$acc_range
    } else {
      dfo <- intervalQueryC(input$dmzDataSets2[1])
      dfl <- intervalQueryC(input$dmzDataSets2[2])
      annotation_range <- rbind(dfo$annotation_range, dfl$annotation_range)
      acc_range <- rbind(dfo$acc_range, dfl$acc_range)
    }
    annotation_range$diff <- difftime(annotation_range$END,annotation_range$START, units='mins')
    acc_range$diff <- difftime(acc_range$END,acc_range$START, units='mins')
    
    output$singlePlot2 <- renderPlot({
      serverSingle$plot2 <- singleplot2(acc_range, annotation_range, seq(0,5, by=input$rangesteps2))
      serverSingle$plot2
    })
    
    output$singlePlot3 <- renderPlot({
      serverSingle$plot3 <- singleplot3(acc_range, annotation_range, input$length2, seq(0,5, by=input$rangesteps2), input$fixedscale2)
      serverSingle$plot3
    })
  })
  
  rlist <- reactive({
    mPlot <- serverSingle$plot2
    aPlot.1 <- serverSingle$plot3
    list(plot = mPlot, height = 4, additionalBaggage = list(list(plot = reactive(aPlot.1), height = 4, info = "Same inputs apply")))
  })
  
  return(rlist)
}

graph3ServerFixed <- function (input, output, session) {
  ns <- session$ns
  
  fixedPlot <- reactiveValues(plot = NULL)
  
  output$fixedintervalslider <- renderUI({
    sliderInputQ(ns, ("fixedinterval1"), "Interval Boundaries:", min = 0, max = 24, value = c(13,17))
  })
  
  #Oberserve any changes to the Databases, reactive variables create problems when assigning diff
  observeEvent(input$dmzDataSets1, {
    if(length(input$dmzDataSets1) == 1) {
      dfo <- intervalQueryC(input$dmzDataSets1[1])
      annotation_range <- dfo$annotation_range
      acc_range <- dfo$acc_range
    } else {
      dfo <- intervalQueryC(input$dmzDataSets1[1])
      dfl <- intervalQueryC(input$dmzDataSets1[2])
      annotation_range <- rbind(dfo$annotation_range, dfl$annotation_range)
      acc_range <- rbind(dfo$acc_range, dfl$acc_range)
    }
    annotation_range$diff <- difftime(annotation_range$END,annotation_range$START, units='mins')
    acc_range$diff <- difftime(acc_range$END,acc_range$START, units='mins')
    
    output$singlePlot1 <- renderPlot({
      fixedPlot$plot <- singleplot1(acc_range, annotation_range, input$fixedinterval1[1], input$fixedinterval1[2])
      fixedPlot$plot
    })
  })
  
  rlist <- reactive({
    mPlot <- fixedPlot$plot
    list(plot = mPlot, height = 4)
  })
  
  return(rlist)
}