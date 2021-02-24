# My first complete app
#'@title Graphical User Interface for XRF data entry.
#'
#'@description This package provide a graphical user interface to enter xrd data easily into the database.
#'@param ui shine app
#'@export
#'@keywords
#'@seealso
#'@return
#'@aliases
#'@examples xrf_entry(ui)

Logged = FALSE;
my_username <- list("Guest")
my_password <- list("1234")

GetTableMetadata <- function() {
  fields <- c(id = "id",
              Planet = "Planet",
              Type = "Type",
              Name1 = "Name1",
              Name2 = "Name2",
              Source = "Source",
              Note = "Note",
              SiO2 = "SiO2",
              TiO2 = "TiO2",
              Al2O3 = "Al2O3",
              Fe2O3 = "Fe2O3",
              FeO = "FeO",
              FeOT = "FeOT",
              MgO = "MgO",
              MnO = "MnO",
              CaO = "CaO",
              Na2O = "Na2O",
              K2O = "K2O",
              P2O5 = "P2O5",
              SO3 = "SO3",
              Cr2O3 = "Cr2O3",
              Cl = "Cl",
              CO2 = "CO2",
              LOI = "LOI",
              TOT = "TOT",
              Date = "Date")


  result <- list(fields = fields)
  return (result)
}

# Find the next ID of a new record
# (in mysql, this could be done by an incremental index)
GetNextId <- function() {
  if (exists("xrf") && nrow(xrf) > 0) {
    max(as.integer(rownames(xrf))) + 1
  } else {
    return (1)
  }
}

#C
CreateData <- function(data) {

  data <- CastData(data)
  rownames(data) <- GetNextId()
  if (exists("xrf")) {
    xrf <<- rbind(xrf, data)
  } else {
    xrf <<- data
  }
}

#R
ReadData <- function() {
  if (exists("xrf")) {
    xrf
  }
}



#U
UpdateData <- function(data) {
  data <- CastData(data)
  xrf[row.names(xrf) == row.names(data), ] <<- data
}

#D
DeleteData <- function(data) {
  xrf <<- xrf[row.names(xrf) != unname(data["id"]), ]
}

mydate <- Sys.Date()  # Get today's date
Date <- format(mydate, format = "%B %d, %Y")


# Cast from Inputs to a one-row data.frame
CastData <- function(data) {
  datar <- data.frame(Planet = data["Planet"],
                      Type = data["Type"],
                      Name1 = data["Name1"],
                      Name2 = data["Name2"],
                      Source = data["Source"],
                      Note = data["Note"],
                      SiO2 = data["SiO2"],
                      TiO2 = data["TiO2"],
                      Al2O3 = data["Al2O3"],
                      Fe2O3 = data["Fe2O3"],
                      FeO = data["FeO"],
                      FeOT = data["FeOT"],
                      MgO = data["MgO"],
                      MnO = data["MnO"],
                      CaO = data["CaO"],
                      Na2O = data["Na2O"],
                      K2O = data["K2O"],
                      P2O5 = data["P2O5"],
                      SO3 = data["SO3"],
                      Cr2O3 = data["Cr2O3"],
                      Cl = data["Cl"],
                      CO2 = data["CO2"],
                      LOI = data["LOI"],
                      TOT = data["TOT"],
                      Date = Date,
                      stringsAsFactors = FALSE)

  rownames(datar) <- data["id"]
  return (datar)
}

# Return an empty, new record
CreateDefaultRecord <- function() {
  mydefault <- CastData(list(id = "0", Planet = "Earth", Type = "Rock",Name1 = "NA", Name2 = "NA", Source = "NA", Note = "NA", SiO2 = "0", TiO2 = "0", Al2O3 = "0", Fe2O3 = "0", FeO = "0", FeOT = "0", MgO = "0", MnO = "0", CaO = "0", Na2O = "0", K2O = "0", P2O5 = "0", SO3 = "0", Cr2O3 = "0", Cl = "0", CO2 = "0", LOI = "0", TOT = "0"))
  return (mydefault)
}

# Fill the input fields with the values of the selected record in the table
UpdateInputs <- function(data, session) {
  updateTextInput(session, "id", value = unname(rownames(data)))
  updateTextInput(session, "Planet", value = unname(data["Planet"]))
  updateTextInput(session, "Type", value = unname(data["Type"]))
  updateTextInput(session, "Name1", value = unname(data["Name1"]))
  updateTextInput(session, "Name2", value = unname(data["Name2"]))
  updateTextInput(session, "Source", value = unname(data["Source"]))
  updateTextInput(session, "Note", value = unname(data["Note"]))
  updateTextInput(session, "SiO2", value = unname(data["SiO2"]))
  updateTextInput(session, "TiO2", value = unname(data["TiO2"]))
  updateTextInput(session, "Al2O3", value = unname(data["Al2O3"]))
  updateTextInput(session, "Fe2O3", value = unname(data["Fe2O3"]))
  updateTextInput(session, "FeO", value = unname(data["FeO"]))
  updateTextInput(session, "FeOT", value = unname(data["FeOT"]))
  updateTextInput(session, "MgO", value = unname(data["MgO"]))
  updateTextInput(session, "MnO", value = unname(data["MnO"]))
  updateTextInput(session, "CaO", value = unname(data["CaO"]))
  updateTextInput(session, "Na2O", value = unname(data["Na2O"]))
  updateTextInput(session, "K2O", value = unname(data["K2O"]))
  updateTextInput(session, "P2O5", value = unname(data["P2O5"]))
  updateTextInput(session, "SO3", value = unname(data["SO3"]))
  updateTextInput(session, "Cr2O3", value = unname(data["Cr2O3"]))
  updateTextInput(session, "Cl", value = unname(data["Cl"]))
  updateTextInput(session, "CO2", value = unname(data["CO2"]))
  updateTextInput(session, "LOI", value = unname(data["LOI"]))
  updateTextInput(session, "TOT", value = unname(data["TOT"]))
}

ui1 <- function(){
  tagList(
    div(id = "login",
        wellPanel(textInput("userName", "Username"),
                  passwordInput("passwd", "Password"),
                  br(),actionButton("Login", "Log in"))),
    tags$style(type="text/css", "#login {font-size:10px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
  )}

ui <- (htmlOutput("page"))
server <- function(input, output,session) {

  USER <- reactiveValues(Logged = Logged)

  observe({
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          Id.username <- which(my_username == Username)
          Id.password <- which(my_password == Password)
          if (length(Id.username) > 0 & length(Id.password) > 0) {
            if (Id.username == Id.password) {
              USER$Logged <- TRUE
            }
          }
        }
      }
    }
  })
  observe({
    if (USER$Logged == FALSE) {

      output$page <- renderUI({
        div(class="outer",do.call(bootstrapPage,c("",ui1())))
      })
    }
    if (USER$Logged == TRUE)
    {

      output$page <- renderUI({


        ui2 <- fluidPage( titlePanel("Interactive XRF Data Entry Platform."),h6("gayantha@uwm.edu"),
                          #use shiny js to disable the ID field
                          shinyjs::useShinyjs(),

                          #input fields
                          tags$hr(),
                          fluidRow(column(2,
                          selectInput("Planet", "Planet:", c("Earth" = "Earth", "Moon" = "Moon", "Mars" = "Mars", "Other" = "Other")),
                          selectInput("Type", "Type:", c("Rock" = "Rock", "Soil" = "Soil", "Meteorite" = "Meteorite", "Evaporite" = "Evaporite", "Other" = "Other"))),
                          column(2,
                          textInput("Name1", "Name1:", ""),
                          textInput("Name2", "Name2:", "")),
                          column(2,
                          textInput("Source", "Source:", ""),
                          textInput("Note", "Note:", ""))),
                          hr(),
                          fluidRow(column(2,
                                          textInput("SiO2", "SiO2", ""),
                                          textInput("TiO2", "TiO2", ""),
                                          textInput("Al2O3", "Al2O3", "")),
                                   column(2,
                                          textInput("Fe2O3", "Fe2O3", ""),
                                          textInput("FeO", "FeO", ""),
                                          textInput("FeOT", "FeOT", "")),
                                   column(2,
                                          textInput("MgO", "MgO", ""),
                                          textInput("MnO", "MnO", ""),
                                          textInput("CaO", "CaO", "")),
                                   column(2,
                                          textInput("Na2O", "Na2O", ""),
                                          textInput("K2O", "K2O", ""),
                                          textInput("P2O5", "P2O5", "")),
                                  column(2,
                                          textInput("SO3", "SO3", ""),
                                          textInput("Cr2O3", "Cr2O3", ""),
                                          textInput("Cl", "Cl", "")),
                                  column(2,
                                          textInput("CO2", "CO2", ""),
                                          textInput("LOI", "LOI", ""),
                                          textInput("TOT", "Total:", ""))),
                          actionButton("submit", "Submit"),
                          actionButton("new", "New"),
                          actionButton("delete", "Delete"),
                          downloadButton('downloadData','Download'),
                          shinyjs::disabled(textInput("id", "Id", "0")),
                          hr(),

                          #data table
                          DT::dataTableOutput("xrf",width = 900))

      })
      #}




      #server <- function(input, output, session) {

      # input fields are treated as a group
      formData <- reactive({
        sapply(names(GetTableMetadata()$fields), function(x) input[[x]])
      })

      # Click "Submit" button -> save data
      observeEvent(input$submit, {
        if (input$id != "0") {
          UpdateData(formData())
        } else {
          CreateData(formData())
          UpdateInputs(CreateDefaultRecord(), session)
        }
      }, priority = 1)

      # Press "New" button -> display empty record
      observeEvent(input$new, {
        UpdateInputs(CreateDefaultRecord(), session)
      })

      # Press "Delete" button -> delete from data
      observeEvent(input$delete, {
        DeleteData(formData())
        UpdateInputs(CreateDefaultRecord(), session)
      }, priority = 1)

      # Select row in table -> show details in inputs
      observeEvent(input$xrf_rows_selected, {
        if (length(input$xrf_rows_selected) > 0) {
          data <- ReadData()[input$xrf_rows_selected, ]
          UpdateInputs(data, session)}

      })

      output$downloadData <- downloadHandler(filename = function(){paste(input$xrf, '.csv', sep = '')},
                                             content = function(file){write.csv(xrf, file)})

      # display table
      output$xrf <- DT::renderDataTable({
        #update after submit is clicked
        input$submit
        #myDir <- file.path("/Users/kodi/Dropbox/RBoxSpace/Earth/TecopaXRD")
        #fileName <- sprintf("%s_%s.csv",as.integer(Sys.time()), digest::digest(responses))
        #write.csv(responses, file = file.path(myDir, fileName), row.names = FALSE, quote = TRUE)
        write.csv(xrf, "backupXRF.csv")
        #write.csv(response, file = file.path(myDir,"backup.csv"), row.names = FALSE, quote = TRUE)
        #update after delete is clicked
        input$delete
        ReadData()
      }, server = FALSE, selection = "single",
      colnames = unname(GetTableMetadata()$fields)[-1]
      )
    }
  }
  )}

shinyApp(ui, server)
