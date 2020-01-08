# A collection of commonly used Controls
session <- c()

setUpSession <- function (inputsession) {
  session <<- inputsession
}

#Checkboxgroup or Text about the DB names
#Input: List of DB names, Namespace
#       inputname: name for the input value
#Input-Names: ('dmzDataSets') for list of DB names
dbselector <- function(dblist, ns, inputname = 'dmzDataSets'){
  if(length(dblist) > 1) {
    checkboxGroupInput(ns(inputname), 'Database: ', c(dblist), 
                       selected = isolate({gQV(session, inputname, dblist[1])}))
  }
  else {
    tagList(
      tags$div(class="hidden", checkboxGroupInput(ns(inputname), 'Database:', c(dblist), selected = dblist[1] )),
      tags$div("Database: ", tags$b(dblist[1]))
    )
  }
}


#Click Events for (outputname)s checkboxes
#Input: Input, Output and Namespace of the current UI, 
#       reactivePatientList: needs to be a reactive Variable of the Patients (important for multiple db)
#       inputname: the name that can be used to get all selected Patients
#       outputname: name of the Output to print at
#       buttonname: name of the buttons
#Input-Names: ('dmzDataSets') for list of DB names
patientSelector <- function (input, output, ns, reactivePatientList, inputname = 'selectedPatients', outputname = 'patientOptionCheckBoxes', buttonname = "selectedall", header = "Select Patients:") {
  buttonID <- paste0(buttonname, inputname, "Ign")
  ##### Button Click selectedall ####
  observeEvent(input[[buttonID]], {
    output[[outputname]] <- renderUI({
      patientSelectorUI(output, ns, reactivePatientList, reactivePatientList, inputname, outputname, buttonname, header, init = FALSE)
    })
  })
  
  ##### Button Click unselectedall ####
  observeEvent(input[[paste0("un",buttonID)]], {
    output[[outputname]] <- renderUI({
      patientSelectorUI(output, ns, reactivePatientList, function () {NULL}, inputname, outputname, buttonname, header, init = FALSE)
    })
  })
}

#Checkboxes for Patient selection, input$(inputname) stores the list of all selected Patients
#Input: Output and Namespace of the current UI, 
#       reactivePatientList: needs to be a reactive Variable of the Patients (important for multiple db)
#       inputname: the name that can be used to get all selected Patients
#       outputname: name of the Output to print at
#       buttonname: name of the buttons
patientSelectorUI <- function (output, ns, reactivePatientList, selection, inputname = 'selectedPatients', outputname = 'patientOptionCheckBoxes', buttonname = "selectedall", header = "Select Patients:", init = TRUE) {
  buttonID <- paste0(buttonname, inputname, "Ign")
  if(init) {
    sele <- isolate({gQV(session, inputname, selection())})
  }
  else {
    sele <- selection()
  }
  output[[outputname]] <- renderUI({
    tagList(
      tags$div(class="col-sm-6", 
               checkboxGroupInput(ns(inputname), header, reactivePatientList(), inline = TRUE, selected = sele)
      ),
      tags$div(class="col-sm-6", 
               actionButton(class="btn-block btn-select", ns(buttonID), ""),
               actionButton(class="btn-block btn-unselect", ns(paste0("un",buttonID)), "")
      )
    )
  })
} 

infoText <- function (ns, text) {
  tagList(
    tags$button(class="help-text-btn pull-right", tags$i(class='fa fa-info')),
    tags$div(class="help-text controlpanel hidden", text)
  )
}

# Checks if input has a query entry, need to use isolate
# Example isolate({gQV(session, inputname, selection())})
gQV <- function (session, name, standard) {
  query <- parseQueryString(session$clientData$url_search)
  if (name %in% names(query) ) { 
    data <- URLdecode(query[[name]])
    if(grepl(',', data)) {
      as.list(strsplit(data, ",")[[1]])
    }
    else {
      data
    }
  }
  else {
    standard
  }
}

sliderInputQ <- function (ns, id, label = "", min, max, value, step = 1) {
  sliderInput(ns(id), label, min = min, max = max,  step = step, value = isolate({gQV(session, id, value)}))
}

checkboxGroupInputQ <- function(ns, id, label, values, selected, inline = TRUE) {
  checkboxGroupInput(ns(id), label, values, inline = inline, selected = isolate({gQV(session, id, selected)}))
}

checkboxInputQ <- function(ns, id, label, value) {
  checkboxInput(ns(id), label, value = isolate({gQV(session, id, value)}))
}

