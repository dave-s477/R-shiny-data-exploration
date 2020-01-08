############################# UI FUNCTION #############################
Graph3UI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      id = 'thirdPlot',
      infoText(ns,                
               tagList(
                  tags$b("Data Availability Explorer"),
                  tags$br(),
                  "This page allows you to examine the availability of sensor data for specific daytimes.",
                  tags$br(),
                  "The first tab shows a Plot with aggregated Data to check if there are enough comparable days.",
                  tags$br(),
                  "The second tab shows the amount of sensor intervals for a choosen daytime interval.",
                  tags$br(),
                  "The third tab includes two Plots with the amount and distribution of the length of sensor intervals"
      )),
      downloadButton(ns("pdfSave"), class="pull-right", "Save Page as PDF"),
      h1("Data Availability"),
      tabsetPanel(id = ns('subtabs'),
                  tabFixed(ns), tabFloat(ns), tabAggr(ns)

      )
    )
  )
}

tabAggr <- function (ns) {
  tabPanel("Aggregated Interval", value = 'aggr',
           column(12,
                  fluidRow(
                    class="controlElements middleLevelControl",
                    column(2,
                           dbselector(dmzDatabases, ns)
                    ),
                    column(4,
                           uiOutput(ns('patientOptionCheckBoxes'))
                    ),
                    column(4, offset = 2,
                           uiOutput(ns('daySlider'))
                    ),
                    column(4,
                           uiOutput(ns('debug0'))
                    )
                  ),
                  fluidRow(
                    plotOutput(ns("aggrPlot"))
                  ) 
           )
  )
}

tabFixed <- function (ns) {
  tabPanel("Fixed Interval", value = 'fixed', 
           column(12,
                  fluidRow(
                    class="controlElements middleLevelControl",
                    column(2,
                           dbselector(dmzDatabases, ns, "dmzDataSets1")
                    ),
                    column(4, offset = 2,
                           uiOutput(ns('fixedintervalslider'))
                    )
                  ),
                  fluidRow(
                    plotOutput(ns("singlePlot1"))
                  )
           ) 
  )
}

tabFloat <- function (ns) {
  tabPanel("Floating Interval", value = 'float', 
           column(12,
                  fluidRow(
                    class="controlElements middleLevelControl",
                    column(2,
                           dbselector(dmzDatabases, ns, "dmzDataSets2")
                    ),
                    column(4,
                           uiOutput(ns("settings"))
                    ),
                    column(4, offset = 2,
                           uiOutput(ns("intervalslider")),
                           uiOutput(ns('intervalLength'))
                    )
                  ),
                  fluidRow(
                    plotOutput(ns("singlePlot3"))
                  ),
                  fluidRow(
                    plotOutput(ns("singlePlot2"))
                  ) 
           ) 
  )
}