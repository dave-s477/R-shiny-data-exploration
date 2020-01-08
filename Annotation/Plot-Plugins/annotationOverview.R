# AnnotationOverviewUI is called to setup the UI of the annotation overview module.
annotationOverviewUI <- function(id){
  # Creation of the namespace
  # All UI elements we create in this modules have to be encapsulated in the namespace
  ns <- NS(id)
  
  # Using a taglist to return multiple UI elements as a single return element
  tagList(
    # The layout is a bootstrap based row and column layout
    fluidRow(
      # Info text, that explains the main uses of the current page
      infoText(ns, 
               tagList(
                 tags$b("Average Annotation Overview:"),
                 tags$br(),
                 "This page allows you to quickly get an overview of the annotation data collected in the inside studies.",
                 tags$br(),
                 "The plots show the average amount of behaviour by category, which was annotated in the selected time span. ",
                 tags$br(),
                 "This explorer is available for DZNE as well as DCM categories, for which less data was collected. ",
                 tags$br(),
                 "Adjustable parameters are the sorting logic and the time slot of interest, that can be selected by the hour in the compare view. ",
                 tags$br(),
                 "Downloadable reports of this page can be generated with the 'Save as PDF' Button, which includes all currently shown plots and the selected parameters."
               )
      ),
      downloadButton(ns("pdfSave"), class="pull-right", "Save Page as PDF"),
      column(9,
             h1("Annotation Overview", align="left")
      )
    ),
    fluidRow(
      column(12,
        fluidRow(
          class = 'controlElements middleLevelControl',
          h4("General options:"),
          column(3,
                 dbselector(dmzDatabases, ns, "dataLocation")
          ),
          column(4,
                 sortingOptions <- uiOutput(ns('sortingOptions'))
          ),
          column(3,
                 HTML("<strong>Sorting Logic:</strong>"),
                 checkboxInput(ns("sortDescending"), "Sort decreasing", value = isolate({gQV(session, 'sortDescendingr',FALSE)}))
          ),
          column(2,
                 numericInput(ns("plotWidth"), "Adjust plot height (pixel per bar):", value = isolate({gQV(session, 'plotWidth',30)}), min = 10, max = 150)
          )
        )
      ),
      fluidRow(
        column(12,
          uiOutput(ns("distPlot")),
          uiOutput(ns("barchart_click_html"))
        )
      )
    ),
    
    fluidRow(
      radioButtons(ns('compareIgn'), h2("Enter Compare View"),c("show", "hide"), selected = "hide"),
      align = "center"
    ),
    # Panel that shows a load animation if shiny is currently busy
    conditionalPanel(
      condition="$('html').hasClass('shiny-busy')", tags$span(class="loader",tags$span(class="loader-inner"))
    ),
    # Panel that shows a additional view depended on the state of a given radio button
    conditionalPanel(
      sprintf("input['%s'] == 'show'", ns("compareIgn")),
      fluidRow(
          # Calling nested module UI with distinct namespaces
          ## NOTE: shared prefixes are do not cause side-effects
          annotationOverviewCompareUI(ns("CompareView1")),
          annotationOverviewCompareUI(ns("CompareView2"))
      )
    )
  )
}

#Define the server logic for the average module -> colorscheme is passed from the server
annotationOverview <- function(input, output, session, colorS, categories, type = 'DZNE'){
  # We have to get the current namespace from the session if we want to create dynamic UI 
  ns <- session$ns
  
  # Creating a list that is used to maintain return values of called modules
  overview.pdf <- list()
  
  # Reactive data is recalculated every time a reactive element in the statement changes.
  # In this case every time the input element dataLocation changes annotationR is recalculated.
  # A change in a reactive element triggers reactions in all statements, which access its value.
  # Reactions can be suppressed with setting the scope "isolated({})"
  ## Reactive value holding the main data used by the current page.
  annotationR <- reactive({
    chosenDmzData(input$dataLocation, type)
  })
  
  # Rendering dynamic UI elements that are dependend on the loaded data
  output$sortingOptions <- renderUI({
    if(is.null(annotationR())){
      return(helpText("Es sind aktuell keine Daten ausgewählt."))
    }
    options <- names(annotationR())[-1]
    tags$div(align = 'left', 
             class = 'multiradio', 
             radioButtons(ns('selectedSortingOrder'), 'Sort by: ', options, selected = isolate({gQV(session, 'selectedSortingOrder',options[1])}),
                          inline   = FALSE))
    
  })
  
  ## Reactive value that restructures the main data dependend on changes in the main data and further input values.
  reactiveAfm <- reactive({
    restructureDmzData(annotationR(), input$selectedSortingOrder, input$sortDescending, type)
  })
  
  # Reactive value in which the current plot height is stored
  plotHeight <- reactive({
    if(is.null(annotationR())){
      return(0)
    }
    (length(unique(annotationR()$NAME)) * input$plotWidth) + 70
  })
  
  # The Plot is calculated as a reactive, the calculated plot is going to be used on multiple occasions,
  # therefore it is calculted before actually rendering it.
  rPlot <- reactive({
    dmzAveragePlot(reactiveAfm(), colorS, categories)
  })
  
  # The UI containing the plot is rendered dynamically. 
  # This is done because the plot height can not be dynamically adjusted, but the height of the UI can. 
  # So setting up the UI dynamically is actually resizing the plot to a fitting value.
  output$distPlot <- renderUI({
    plotOutput(ns("renderedPlot"), click = ns("dmz_barchartIgn"), height = paste0(plotHeight(), "px"))
  })
  
  # The actual plot is rendered in the setup UI
  output$renderedPlot <- renderPlot({
    rPlot()
  })
  
  # This function renders additional information
  output$barchart_click_html <- renderUI({
    dmzAverageClick(reactiveAfm(), input$dmz_barchartIgn, colorS)
  })
  
  # The following reactives are setup to directly trigger reactions when passed on to a nested module
  # Passing on the inputs would only pass on the current value and not have a reactive effect.
  ############################
  dataOption <- reactive({
    input$dataLocation
  })
  
  sortOrder <- reactive({
    input$selectedSortingOrder
  })
  
  desc <- reactive({
    input$sortDescending
  })
  ############################
  
  # Calling the server logic of nested modules and saving return values in the initially setup list
  overview.pdf[[2]] <- callModule(annotationOverviewCompare, "CompareView1", annotation = annotationR, colors = colorS, dmzDataOptions = dataOption, sortingOrder = sortOrder, sortdesc = desc, categorieS = categories, type = type)
  overview.pdf[[3]] <- callModule(annotationOverviewCompare, "CompareView2", annotation = annotationR, colors = colorS, dmzDataOptions = dataOption, sortingOrder = sortOrder, sortdesc = desc, categorieS = categories, type = type)
  
  # Handling clicks on the download-button
  # To override standard behaviour we implement a specific download handler
  output$pdfSave <- downloadHandler(
    filename = "annotationOverview.pdf",
    content = function(file){
      # Adjusting the values in the returnlist in order to fit the format in which they are further processed
      overview.pdf[[1]] <- list(plot = rPlot(), height = length(unique(annotationR()$NAME))/2+0.5)
      overview.pdf[[2]] <- overview.pdf[[2]]()
      overview.pdf[[3]] <- overview.pdf[[3]]()
      # Setting up a list of parameters that are passed to the PDF generation
      params <- list(title = "Annotation Overview", inputs = reactiveValuesToList(input), plots = overview.pdf)
      # Rendering a PDF document with the rules given in the Rmarkdown file "reportGenerator.Rmd"
      rmarkdown::render("Rmds/reportGenerator.Rmd", output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
    }
  )
}

# Handling the querying of data for the case that there are multiple databases, that hold relevant information
chosenDmzData <- function(databases, type){
  if (length(databases) == 0)
    return(NULL)
  
  annotation <- dmzDataTransform(dmzBarchartQuery(databases[1], type))
  if(length(databases) > 1){
    for(i in 2:length(databases)){
      annotation_next_set <- dmzDataTransform(dmzBarchartQuery(databases[i], type))
      annotation <- rbind(annotation, annotation_next_set)
    }
  }
  annotation
}

# Transform the queried data in a suitable format with the help of the dplyr package
dmzDataTransform <- function(annotation_data){
  if(length(annotation_data[,1]) == 0){
    return(NULL)
  }
  annotation_data %>%
    dplyr::mutate(NAME=factor(NAME), CATEGORY=factor(ifelse(is.na(CATEGORY), "None", CATEGORY))) %>%
    reshape2::dcast(., NAME~CATEGORY, fill = 0, value.var = 'freq') ->     
    annotation_out
  return(annotation_out)
}

# Function that gets the data for an annotation overview plot from a database
dmzBarchartQuery <- function(dbName, type){
  conn <- dbConnect(MonetDB.R(), host="localhost", dbname=dbName, user="monetdb", password="password")
  if(type == "DZNE"){
    query <- "with n_count as (select \"PATIENT$ID\", count(*) as a from extendedannotation group by \"PATIENT$ID\"), 
    nc_count as (select \"PATIENT$ID\", \"CATEGORY\", count(*) as b from extendedannotation group by \"PATIENT$ID\", \"CATEGORY\") 
    select \"NAME\", \"CATEGORY\", cast(nc_count.b as real)/cast(n_count.a as real) as freq from n_count inner join nc_count on n_count.\"PATIENT$ID\"=nc_count.\"PATIENT$ID\" inner join raw.patient rp on rp.\"ID\"=nc_count.\"PATIENT$ID\";"
  } else if (type == "DCM"){
    query <- "with n_count as (select \"PATIENT$ID\", count(*) as a from raw.annotation where \"TYPE\"='DCM' group by \"PATIENT$ID\"), 
    nc_count as (select \"PATIENT$ID\", \"CATEGORY\", count(*) as b from raw.annotation where \"TYPE\"='DCM' group by \"PATIENT$ID\", \"CATEGORY\") 
    select \"NAME\", \"CATEGORY\", cast(nc_count.b as real)/cast(n_count.a as real) as freq from n_count inner join nc_count on n_count.\"PATIENT$ID\"=nc_count.\"PATIENT$ID\" inner join raw.patient rp on rp.\"ID\"=nc_count.\"PATIENT$ID\";" 
  }
  queriedData <- dbGetQuery(conn, query)
  dbDisconnect(conn)
  return(queriedData)
}

# Further transformations that are depended on additional parameters
restructureDmzData <- function(data, sortingOrder, sortdesc, type){
  if(is.null(data)) {
    return(NULL)
  }
  # sort category levels by relative amount of occurence
  ordering <- colSums(data[,-1])
  ordering[sortingOrder] <- 1000
  data.levels <- names(data)[-1][order(ordering, decreasing = FALSE)]
  # sort subjects by their relative amount of category A
  x.levels <- data$NAME[order(data[sortingOrder], decreasing = sortdesc)]  #eval(parse(text=paste("data$", ifelse(is.null(sortingOrder), "A", sortingOrder), sep="")))
  
  # ensure category 'none' is printed on negative side of axis
  if(type == "DZNE"){
    data$None <- -data$None
  }
  
  # reformat data
  afm <- reshape2::melt(data, id.vars='NAME')
  # actually sort names
  afm$NAME <- factor(afm$NAME, levels=x.levels)
  # actually sort categories
  afm$variable <- factor(afm$variable, levels=data.levels)
  return(afm)
}

# Creating a plot from restructured queryied data 
dmzAveragePlot <- function(plot_data, colorScheme, categories, dropGuides = FALSE, type){
  p <- ggplot(plot_data, aes(NAME, value)) + 
    geom_abline(intercept = c(-.5,.5), slope = 0, color='darkgray') + 
    geom_bar(aes(fill=variable), position='stack', stat='identity') +
    theme(legend.position = 'top') +  
    #scale_fill_manual(values = colorScheme) +
    scale_fill_manual(
      values = colorScheme,
      breaks = categories,
      labels = categories
    ) +
    xlab("Proband") + 
    ylab("Anteil") +  
    guides(fill=guide_legend("Kategorien", nrow=1, byrow=FALSE)) +
    scale_y_continuous(breaks=c(-.5,0,.5), labels = c("50%","0%","50%")) +
    coord_flip()
  if(dropGuides){
    p <- p + theme(legend.position = "none")
  } 
  return(p)
}

# Dynamically render HTML output if a click on the barchart is registered
dmzAverageClick <- function(data, click, colorScheme){
  if (is.null(click$y)){
    html <- HTML("<div style='background:white', align='center'><strong>Click on patient for exact percentage values.</strong></div>")
    return(html)
  }
  else {
    lvls <- levels(data$NAME) 
    name <- lvls[round(click$y)]
    pos <- match(name, data$NAME)
    outputProband <- paste0("<div style='background:white', align='center'><strong>Selected proband: ", name, "</strong><ul>")
    probandValues <- data$value[seq(pos, length(data$value), length(lvls))]
    neworder <- as.vector(unique(data$variable))
    newColorScheme <- colorScheme[sort(neworder)]
    outputValues <- paste0("<li style=color:",
                           colorScheme[sort(neworder)],
                           ">",
                           unique(as.vector(data$variable)), 
                           ": ", 
                           percent(abs(probandValues)),
                           "</li>",
                           collapse = ""
    )
    html <- HTML(paste0(outputProband, outputValues, "</ul></div>"))
    return(html)
  }
}
