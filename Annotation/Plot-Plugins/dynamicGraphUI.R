dynamicGraphUI <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      id = 'thirdPlot',
      sidebarLayout(
        mainPanel(
          width=9
          #plotOutput(ns("plot"))
        ),
        sidebarPanel(
          width=3,
          checkboxGroupInput(ns('dmzDataSets'), 'Database: ', c(dmzDatabases), selected = dmzDatabases[1] ),
          selectInput(ns("x-axis"), 'X-axis: ', 
                      choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                      selected = 1),
          selectInput(ns("y-axis"), 'Y-axis: ', 
                      choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                      selected = 1),
          
          hr(),
          fluidRow(column(3, verbatimTextOutput("value")))
        )
      )
    ),
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",tags$div(class="overlay",tags$div(class="loader")))
  )
}

dynamicGraph <- function(input, output, session){
  ns <- session$ns
  
  output$value <- renderPrint({ input$x-axis })
  
  output$plot <- renderPlot({
#    annotationAmountPlot(true);
  })
}

########################################Plot########################################

#annotationAmountPlot <- function(plot_data){
#  return true;
#}