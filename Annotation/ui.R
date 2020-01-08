library(shiny)
library(shinydashboard)
options(shiny.sanitize.errors = FALSE)

#Load config
source("database_config.R")

#Load Plugins for plotting, further info in server.R
source("Plot-Plugins/annotationOverview.R")
source("Plot-Plugins/annotationOverviewCompare.R")
source("Plot-Plugins/annotationSequence.R")
source("Plot-Plugins/annotationSequencePlot.R")
source("Plot-Plugins/motionScore.R")
source("Plot-Plugins/motionScoreFilter.R")
source("Plot-Plugins/motionScorePlot.R")
source("Plot-Plugins/Graph3UI.R")

#Include shared Controls
source("Plot-Plugins/controls.R")

dashboardPage(
  dashboardHeader(title = "Inside Explorer"),
  dashboardSidebar(
    sidebarMenu(id="tabs",
                #Sidebar is serverd by server.R
                sidebarMenuOutput("menu")
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$script(src="basis.js")
    ),
    tabItems(
      tabItem(tabName = "first", fluidPage(annotationOverviewUI("Overview-DZNE"))),
      tabItem(tabName = "first_second", fluidPage(annotationOverviewUI("Overview-DCM"))),
      tabItem(tabName = "second", annotationSequenceUI("Sequence-DZNE")),
      tabItem(tabName = "second_second", annotationSequenceUI("Sequence-DCM")),
      tabItem(tabName = "three", fluidPage(Graph3UI("three"))),
      tabItem(tabName = "fourth", motionScoreUI("Motion-Score"))
    ),
    conditionalPanel(
      condition = "!output.setupComplete", tags$div(class="start-loader", tags$span(class="loader-msg", tags$h2("Starting Session...")))
    ),
    conditionalPanel(
      condition="$('html').hasClass('shiny-busy')", tags$span(class="loader",tags$span(class="loader-inner"))
    )
  )
)
