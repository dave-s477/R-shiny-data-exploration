annotationSequenceUI <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      infoText(ns, 
               tagList(
                 tags$b("Annotation Sequence Explorer:"),
                 tags$br(),
                 "This page allows you to examine the change in annotation sequence over the entire time of the inside studies.",
                 tags$br(),
                 "There are two views on the data for each of the different collection phases of the study.",
                 tags$br(),
                 "The first view on the data is patient-centered and can be used to explore changes in patient behaviour over the time of the study. ",
                 tags$br(),
                 "The second view is observer-centered and allows to search for time slots in which multiple observeres annotated for the same patient.",
                 tags$br(),
                 tags$br(),
                 tags$b("Zooming"),
                 tags$br(),
                 "Aside from normal selection options all plots on this page allow zooming options.",
                 tags$br(),
                 "Zooming is accomplished by brushing a rectangle on a plot. Clicking on a plot will remove the last selected brush.",
                 tags$br(),
                 "It is possible to adjust the rectangle by dragging or stretching it.",
                 tags$br(),
                 tags$br(),
                 "On main plots, zooming will filter the selected patients and the selected time slot. ",
                 tags$br(),
                 "If start or end of any time slot lies between two days, the slot is moved to the beginning of the next, or the end of the last day.",
                 tags$br(),
                 "If interesting data was selected, a zoom can be saved with the 'Fix Zoom' button. ",
                 tags$br(),
                 "The last added Zoom can also be removed with the 'Remove Zoom' button. ",
                 tags$br(),
                 "Fixed zooms are also zoomable. On fixed plots, zooming is accomplished by brushing a rectangle and double-clicking on the selected plane.",
                 tags$br(),
                 "Zooming on already fixed plots does only adjust the time slot, no further patient selection."
               )
               ),
      downloadButton(ns("pdfSave"), class="pull-right", "Save Page as PDF"),
      h1("Annotation Sequence Explorer")
      ),
    fluidRow(
      class = 'controlElements middleLevelControl',
      column(3,
             helpText("Load the data for the compare view. This might take a view seconds.")
             ),
      column(3,
             actionButton(ns("show"), "Initial Load", icon = icon("bar-chart"))
             ),
      column(3,
             helpText("Only fixed Zooms will be included in the PDF document. They are included with the current Zoom depth.")
             ),
      column(3)
    ),
    fluidRow(
      verbatimTextOutput(ns("debug")),
      tabsetPanel(
        #tabPanel("Single Patient", dmzAnnotationTimeCourseUI(ns("single"))),
        tabPanel("Patient Centered View", 
                 uiOutput(ns('pcv'))
        ),
        tabPanel("Observer Centered View", 
                 uiOutput(ns('ocv'))
        )
      )
    )
  )
}

annotationSequence <- function(input, output, session, colorS, categories, type = "DZNE"){
  ns <- session$ns
  
  annotation_set <- chosenAnnotationTimeData(dmzDatabases, type)
  
  sequence.pdf <- list()
  sequence.setup.iterator <- 0
  sequence.iterator <- 0
  
  observeEvent(input$show, {
    if(length(annotation_set) == 0){
      
    } else {
      output$pcv <- renderUI({
        lapply(1:length(annotation_set), function(k) {
          sequence.setup.iterator <<- sequence.setup.iterator + 1 
          annotationSequencePlotUI(ns(paste0("pcv", sequence.setup.iterator)))
        })
      })
      output$ocv <- renderUI({
        lapply(1:length(annotation_set), function(k) {
          sequence.setup.iterator <<- sequence.setup.iterator + 1
          annotationSequencePlotUI(ns(paste0("ocv", sequence.setup.iterator)))
        })
      })
    }
  })
  
  observeEvent(input$show, {
    if(length(annotation_set) == 0){
      
    } else {
      lapply(1:length(annotation_set), function(k) {
        if(nrow(annotation_set[[k]]) > 0){
          sequence.iterator <<- sequence.iterator + 1
          sequence.pdf[[sequence.iterator]] <<- callModule(
            annotationSequencePlot,
            paste0("pcv", sequence.iterator),
            annotation_set[[k]],
            categories = categories,
            colors = colorS
          )
        }
      })
      lapply(1:length(annotation_set), function(k) {
        if(nrow(annotation_set[[k]]) > 0){
          sequence.iterator <<- sequence.iterator + 1
          sequence.pdf[[sequence.iterator]] <<- callModule(
            annotationSequencePlot,
            paste0("ocv", sequence.iterator),
            annotation_set[[k]],
            categories = categories,
            colors = colorS,
            type = "observer"
          )
        }
      })
    }
  })
  
  output$pdfSave <- downloadHandler(
    filename = "annotationSequence.pdf",
    content = function(file){
      for(i in 1:length(sequence.pdf)){
        sequence.pdf[[i]] <- sequence.pdf[[i]]()
      }
      params <- list(title = "Annotation Sequence", mainPlotExists = FALSE, inputs =  reactiveValuesToList(input), plots = sequence.pdf)
      rmarkdown::render("Rmds/reportGenerator.Rmd", output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
    }
  )
}

dmzSinglePatientQuery <- function(dbName, type){
  conn <- dbConnect(MonetDB.R(), host="localhost", dbname=dbName, user="monetdb", password="password")
  query <- paste0("select a.\"START\", \"NAME\", a.\"END\", \"CATEGORY\", \"OBSERVER\", \"location\" from raw.annotation a inner join location l on a.\"PATIENT$ID\"=l.\"PATIENT$ID\" where \"TYPE\"='", type,"'")
  queriedData <- dbGetQuery(conn, query)
  dbDisconnect(conn)
  return(queriedData)
}

chosenAnnotationTimeData <- function(databases, type){
  annotation.df <- NULL
  for (i in databases) {
    next.df <- dmzSinglePatientQuery(i, type)
    next.df %>%
      dplyr::mutate(START=as.POSIXct(START), 
                    END=as.POSIXct(END),
                    NAME=factor(NAME), 
                    OBSERVER=factor(OBSERVER), 
                    CATEGORY=factor(ifelse(is.na(CATEGORY), 'None',CATEGORY)),
                    location=factor(location)) %>%
      dplyr::mutate(day=date(START)) ->
      next.df
    annotation.df <- rbind(next.df, annotation.df)
  }
  annotationList <- split(annotation.df, annotation.df$location)
  return(annotationList)
}
