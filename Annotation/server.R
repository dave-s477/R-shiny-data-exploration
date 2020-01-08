options(shiny.sanitize.errors = FALSE)

#Required packages are loaded in the server file and are also availiable in the UI.R
library(shiny)
library(shinydashboard)
library(dplyr)
library(lubridate)
library(MonetDB.R)
library(DBI)
library(magrittr)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(parallel)
library(mgcv)
library(reshape2)
library(memoise)

# Setting up the database configuration
source("database_config.R")

# Include shared Controls
source("Plot-Plugins/controls.R")

# Load function for fixed colorscheme creation
source("color_scheme.R")

# Load modules used in the app:
## The app is implemented with shiny modules. Each module has its own associated user interface and server logic. 
## Modules can be called anywhere in shiny code and are required to create a new distinct namespace. 
## NOTE: module namespaces should not have a prefix relation: namespace1 = "first" u. namespace2 = "firstAddition", would have undesired side-effects!
# There are four main modules that implement the logic of on explorer site and call nested modules themselfs:
source("Plot-Plugins/annotationOverview.R")
source("Plot-Plugins/annotationSequence.R")
source("Plot-Plugins/Graph3Server.R")
source("Plot-Plugins/motionScore.R")
# The following implement the logic of nested modules:
source("Plot-Plugins/annotationOverviewCompare.R")
source("Plot-Plugins/annotationSequencePlot.R")
source("Plot-Plugins/motionScoreFilter.R")
source("Plot-Plugins/motionScorePlot.R")

# Some explorer sites share a hardcoded color scheme, that is used to keep consistent colors troughout the entire app.
categoriesDZNE <- c("Ag", "O", "G", "P", "M", "A", "None")
categoriesDCM <- c("X", "Z", "O", "T", "U", "P", "E", "L", "Y", "W", "N", "D", "V", "C", "A", "F", "K", "B", "Q")
colorSchemeDZNE <- brewer.pal(length(categoriesDZNE), 'Paired')
names(colorSchemeDZNE) <- categoriesDZNE
colorSchemeDCM <- brewer.pal(12, 'Set3')
colorSchemeDCM <- c(colorSchemeDCM, brewer.pal(length(categoriesDCM)-12, 'Set2'))
names(colorSchemeDCM) <- categoriesDCM
extendedColorScheme <- extendBasicDmzScheme()
extendedCategories <- c("Ag", "O", "G", "P", "M", "A", "None", "Q")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  #re-source for better debugging..
  source("color_scheme.R")
  source("Plot-Plugins/annotationOverview.R")
  source("Plot-Plugins/annotationOverviewCompare.R")
  source("Plot-Plugins/annotationSequence.R")
  source("Plot-Plugins/annotationSequencePlot.R")
  source("Plot-Plugins/motionScore.R")
  source("Plot-Plugins/motionScoreFilter.R")
  source("Plot-Plugins/motionScorePlot.R")
  source("Plot-Plugins/Graph3Server.R")
  source("Plot-Plugins/controls.R")
  
  setUpSession(session)
  
  #Sidebar Panel, need to be in Server for the Tab selection
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Annotation Overview (DZNE)", tabName = "first", icon = icon("bar-chart-o")),
      menuItem("Annotation Overview (DCM)", tabName = "first_second", icon = icon("bar-chart-o")),
      menuItem("Annotation Sequence (DZNE)", tabName = "second", icon = icon("bar-chart-o")),
      menuItem("Annotation Sequence (DCM)", tabName = "second_second", icon = icon("bar-chart-o")),
      menuItem("Data Availability", tabName = "three", icon = icon("bar-chart-o")),
      menuItem("Motion Score (AMS)", tabName = "fourth", icon = icon("line-chart"))
    )
  })
  isolate({
    if(session$clientData$url_hash != '') {
      split <- strsplit(session$clientData$url_hash, "-")
      updateTabItems(session, "tabs", tail(split[[1]], 1))
    }
    else if(session$clientData$url_search != '') {
      query <- parseQueryString(session$clientData$url_search)
      if(!is.null(query[["tab"]])) {
        updateTabItems(session, "tabs", query[["tab"]])
      }
    }
  })

  # The server logic of modules is called with the function callModule
  ## Call of annotationOverview modules, which create the first two explorer sites.
  callModule(annotationOverview, "Overview-DZNE", colorS = colorSchemeDZNE, categoriesDZNE)
  callModule(annotationOverview, "Overview-DCM", colorS = colorSchemeDCM, categoriesDCM, type = "DCM")
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## Call of annotationSequence modules, which implement 3rd and 4th explorer sites.
  callModule(annotationSequence, "Sequence-DZNE", colorS = extendedColorScheme, extendedCategories)
  callModule(annotationSequence, "Sequence-DCM", colorS = colorSchemeDCM, categoriesDCM, type = "DCM")
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## Call of available data module, which implements 5th explorer site.
  callModule(graph3Server, "three")
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## Call of motion score module, which implements 6th explorer site.
  callModule(motionScore, "Motion-Score")
  
  rv <- reactiveValues()
  rv$setupComplete <- FALSE
  
  ## simulate data load
  observe({
    rv$setupComplete <- TRUE
  })
  
  output$setupComplete <- reactive({
    return(rv$setupComplete)
  })
  outputOptions(output, 'setupComplete', suspendWhenHidden=FALSE)
})
