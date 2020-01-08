motionScoreUI <- function(id){
  ns <- NS(id)
  
  fluidPage(
    tags$head(tags$script(HTML(paste0("$(function() {
        var loadedData = 0;
        var trackedPlots = [];
        $( '#", ns("load") ,"' ).click(function() {
            var selectedPatients = $( 'input[name=", ns("selectedPatients"), "]:checked').size();
            if (selectedPatients > 0){
                loadedData = loadedData + 1;
                trackedPlots.push(loadedData);
                var tabId = Math.floor(Math.random() * (900000 - 100000) + 100000);
                $( \"li\" ).each(function(){
                    $(this).removeClass('active');
                })
                $( '#", ns("tabbing-content"), "' ).each(function(){
                    $(this).find('div').each(function(){
                        $(this).removeClass('active');
                    })
                })
                $( '#", ns("tabbing"), "' ).append(\"<li class='active'><a href='#\" + tabId + \"' data-toggle='tab'>Data \" + loadedData + \"</a></li>\");
                $( '#", ns("tabbing-content"), "' ).append(\"<div class='tab-pane active' id='\"+ tabId + \"'><div id='", ns("data"), "\" + loadedData + \"'></div></div>\");
            }
            Shiny.onInputChange('", ns("trackedPlots"),"', trackedPlots.toString());
        });
        $( '#", ns("remove") ,"' ).click(function(){
            var removedID = -1;
            $( \"li\" ).each(function(){
                if( $(this).hasClass('active') ) {
                    $(this).find('a').each(function(){
                        removedID = parseInt($(this).html().split(' ')[1]);
                    })
                    $(this).remove();
                }
            })
            $( '#", ns("tabbing-content"), "' ).each(function(){
                $(this).find('div').each(function(){
                    $(this).hasClass('active');
                    if( $(this).hasClass('active') ){
                        $(this).remove();
                    }
                })
            })
            if(removedID >= 0){
                var removedPlotPos = trackedPlots.indexOf(removedID);
                trackedPlots.splice(removedPlotPos, 1)
            }
            Shiny.onInputChange('", ns("trackedPlots"),"', trackedPlots.toString());
        })
    });")))),
    
    fluidRow(
      infoText(ns, tagList(
                      tags$b("Acceleration Explorer:"),
                      tags$br(),
                      "This explorer page allows you to view the AMS acceleration data collected in the inside experiments.",
                      tags$br(),
                      "The data amount available is quite large and therefore the exploration-process is split three ways to handle the load.",
                      tags$br(),
                      "Because loading and plotting the data might take some time, sampling options are available in those steps.",
                      tags$br(),
                      tags$br(),
                      tags$b("Querying the data"),
                      tags$br(),
                      "In the initial step data of interest is queried and collected from the database.",
                      tags$br(),
                      "Then a new view is opened, on which further possibilities for the selected data are available.",
                      tags$br(),
                      "Performancewise it allows a better workflow, if all the nessecary data for one compare is preloaded and explored in the next steps.",
                      tags$br(),
                      tags$br(),
                      tags$b("Filtering the data"),
                      tags$br(),
                      "The next step allows to apply several different filtering options on the preloaded data.",
                      tags$br(),
                      tags$br(),
                      tags$b("Plotting the data"),
                      "In the last step plotting options can be chosen.",
                      tags$br(),
                      "The available data is split into facets based on the selected plotting aggregation.",
                      tags$br(),
                      "For the facets the scale can be chosen absolute or relativ."
                      )),
      downloadButton(ns("pdfSave"), class="pull-right", "Save Page as PDF"),
      column(9,
             h1("AMS Acceleration Data Explorer")
             ),
      column(3)
    ),
    fluidRow(
      class = 'controlElements topLevelControl', 
      h4("Data loading options:"),
      column(10,
        uiOutput(ns("selectLoading"))
      ),
      column(2,
        actionButton(ns("load"), "Load Data", icon = icon("line-chart")),
        br(),
        br(),
        actionButton(ns("remove"), "Remove current tab", icon = icon("remove"))
      )
    ),
    fluidRow(
      verbatimTextOutput(ns("debug")), 
      HTML(paste0("<div><input type='text' id='", ns("trackedPlots"), "' name='", ns("trackedPlots"), "' style='display: none;'></div>
                  <div class='tabbable'>
                      <ul id ='", ns("tabbing"), "' class='nav nav-pills'>
                      </ul>
                      <div id ='", ns("tabbing-content"), "' class='tab-content col-md-12'>
                      </div>
                  </div>"))
    ),
    conditionalPanel(
      condition="$('html').hasClass('shiny-busy')", tags$span(class="loader",tags$span(class="loader-inner"))
    )
  )
}

motionScore <- function(input, output, session){
  #Get the current session element (required for dynamic UI elements)
  ns <- session$ns
  
  motionScore.pdf <- list()
  
  #Setting up a list of reacitve values to handle patient selection (careful: just values are reactive, not the list itsself)
  dataStack <- reactiveValues(index = 0, lastTrackedPlots = NULL)
  
  patient_opts <- reactive({
    patients()
  })

  #Rendering the patient selection checkboxes
  output$selectLoading <- renderUI({
    tagList(
      column(6,
             uiOutput(ns('patientOptionCheckBoxes'))
             
             ),
      column(3,
             sliderInput(ns("time"), "Select time", min = 0, max = 24, value = c(0,24))
             ),
      column(3,
             sliderInput(ns("sample"), "Sampling fraction", min = 0, max = 1, value = 1, step = 0.01))
    )
  })
  
  patient_options <- reactive({
    patient_opts()$NAME
  })
  
  patientSelector(input, output, ns, patient_options)
  patientSelectorUI(output, ns, patient_options, patient_options)
  
  #Handling behaviour on patient reload
  observeEvent(input$load, {
    #newly selected patients
    patients.2 <- input$selectedPatients
    time <- input$time
    sampling <- input$sample
    
    if(length(patients.2) > 0){
      dataStack$index <- dataStack$index + 1
      currentIndex <- dataStack$index
      
      insertUI(
        #select the placeholder div
        selector = paste0('#', ns("data"), currentIndex),
        ui = tags$div(
          motionScoreFilterUI(ns(as.character(currentIndex))),
          id = ns(as.character(currentIndex))
        ),
        where = "beforeBegin"
      )
      
      singleDataBaseSelect <- list()
      for(i in dmzDatabases){
        singleDataBaseSelect[[i]] <- list()
      }
      
      for(i in patients.2){
        origin <- patient_opts()$origin[patient_opts()$NAME == i]
        singleDataBaseSelect[[origin]] <- c(singleDataBaseSelect[[origin]], i)
      }
      
      data <- NULL
      for(database in names(singleDataBaseSelect)){
        patients.currentDB <- unlist(singleDataBaseSelect[[database]])
        if (length(patients.currentDB > 0)){
          data.next <- load.ams(dbname = database, patients = patients.currentDB, start.hour = time[1], end.hour = time[2], sample.frac = sampling)
          data <- rbind(data.next, data)
        }
      }
      
      motionScore.pdf[[paste0("plot", currentIndex)]] <<- callModule(motionScoreFilter, paste0(as.character(currentIndex)), data, patients.2, time)
    }
  })
  
  observeEvent(input$trackedPlots, {
    trackedPlots <- as.numeric(strsplit(input$trackedPlots, ",")[[1]])
    droppedPlots <- setdiff(dataStack$lastTrackedPlots, trackedPlots)
    if(length(droppedPlots) > 0){
      for (i in droppedPlots){
        motionScore.pdf[[paste0("plot", i)]] <<- NULL
      }
    }
    dataStack$lastTrackedPlots <- trackedPlots
  })
  
  output$pdfSave <- downloadHandler(
    filename = "motionScore.pdf",
    content = function(file){
      motionScoreList <- list()
      explicitPattern <- NULL
      if(length(motionScore.pdf) > 0){
        for(i in 1:length(motionScore.pdf)){
          motionScoreList[[i]] <- motionScore.pdf[[i]]()
        }
        explicitPattern <- dataStack$lastTrackedPlots
      }
      params <- list(title = "Motion Score", mainPlotExists = FALSE, inputs = reactiveValuesToList(input), plots = motionScoreList, explicitPattern = explicitPattern)
      rmarkdown::render("Rmds/reportGenerator.Rmd", output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
    }
  )
}

#Querying all patients that exists in all databases and passing them back in one dataframe
patients <- function(){
  patient.df <- NULL
  if(length(dmzDatabases > 0)) {
    for(k in 1:length(dmzDatabases)) {
      conn <- dbConnect( MonetDB.R(), host = "localhost", dbname = dmzDatabases[k], user = 'monetdb', password = 'password')
      patients <- dbGetQuery(conn, ' select \"NAME\", \"ID\" from raw.patient')
      dbDisconnect(conn)
      patients %>%
        mutate(origin = dmzDatabases[k]) ->
        patients

      patient.df <- rbind(patients, patient.df)
    }
  }
  return(patient.df)
}


load.ams <- function(dbname='database', patients=NULL, start.hour = 0, end.hour=24, sample.frac=.1){
  #first get necessary protocols for this selection
  query <- "select \"NAME\", a.\"ID\" from AMSconfig a inner join raw.protocol p on p.\"ID\"=a.\"PROTOCOL$ID\" inner join raw.patient pa on pa.\"ID\"=p.\"PATIENT$ID\""
  
  if (!is.null(patients)){
    query <- paste0(query, " where \"NAME\" in (\'",paste0(patients, collapse = "\',\'"),"\')")
  }
  
  conn <- DBI::dbConnect(MonetDB.R(), host="localhost", dbname=dbname, user='monetdb', password='password')
  a <- dbGetQuery(conn, query)
  loc <- dbGetQuery(conn, "Select AMSconfig.\"ID\" as \"AMSconfig$ID\" ,\"START\", \"NAME\", location  from location inner join raw.protocol p on p.\"PATIENT$ID\"=location.\"PATIENT$ID\" inner join AMSconfig on AMSconfig.\"PROTOCOL$ID\"=p.\"ID\"") %>%
    dplyr::mutate(day=yday(START), week = week(START)) %>%
    dplyr::select(-START)
  
  
  
  # then get ams data based on protocols ans selection
  ams.query <- paste("select * from ams_tmp where \"AMSconfig$ID\" in (", paste0(a$ID,collapse = ','), ")")
  
  if (start.hour < end.hour){
    compare <- c(">=", "<=")
  }else{
    compare <- c("<=",">=")
  }
  
  ams.query <- paste0(ams.query, " and extract(hour from \"START\") ", compare[1], start.hour)
  ams.query <- paste0(ams.query, " and extract(hour from \"START\") ", compare[2], end.hour)
  if (sample.frac<1 & sample.frac > 0){
    ams.query <- paste(ams.query, "sample" ,sample.frac)
  }
  
  ams <- dbGetQuery(conn, ams.query) %>%
    dplyr::mutate(START=as.POSIXct(START)) %>%
    dplyr::inner_join(loc, by=c("AMSconfig$ID"="AMSconfig$ID")) %>%
    dplyr::mutate(weekday=wday(START,label = TRUE), day=yday(START)-day, week=week(START)-week, NAME=factor(NAME))
}
