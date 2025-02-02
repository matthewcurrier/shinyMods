#' SPSS Data Loader UI Module
#'
#' Creates a UI component for uploading and managing SPSS (.sav) files in a Shiny application.
#'
#' @param id A character string. The module ID used to namespace the UI elements.
#'
#' @return A tagList containing file input and clear button UI elements.
#'
#' @importFrom shiny NS tagList fileInput actionButton
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(bslib)
#'   library(shinyjs)
#'   library(haven)
#'   library(DT)
#'   library(shiny.emptystate)
#'
#'   # Example usage in a Shiny app
#'   ui <- fluidPage(
#'     shinyjs::useShinyjs(),
#'     titlePanel("SAV File Explorer"),
#'     sidebarLayout(
#'       sidebarPanel(
#'         dataLoaderUI("dataLoader")
#'       ),
#'       mainPanel(
#'         use_empty_state(),
#'         verbatimTextOutput("dataInfo"),
#'         tableOutput("dataPreview")
#'       )
#'     )
#'   )
#'
#'   server <- function(input, output, session) {
#'     data <- dataLoaderServer("dataLoader")
#'
#'     observe({
#'       current_data <- data()
#'       if (is.null(current_data)) {
#'         output$dataPreview <- renderTable({ NULL })
#'         output$dataInfo <- renderPrint({ NULL })
#'       } else {
#'         output$dataInfo <- renderPrint({
#'           cat("Dataset Information:\n")
#'           cat("Number of rows:", nrow(current_data), "\n")
#'           cat("Number of columns:", ncol(current_data), "\n")
#'           cat("\nColumn names:\n")
#'           print(names(current_data))
#'         })
#'         output$dataPreview <- renderTable({
#'           head(current_data, 5)
#'         })
#'       }
#'     })
#'   }
#'
#'   shinyApp(ui = ui, server = server)
#' }
dataLoaderUI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), "Upload SAV file",
              accept = c(".sav")),
    actionButton(ns("clear"), "Clear Data")
  )
}

#' SPSS Data Loader Server Module
#'
#' Creates a server component for handling SPSS (.sav) file uploads and data management
#' in a Shiny application.
#'
#' @param id A character string. The module ID that corresponds to the UI element.
#'
#' @return A reactive expression containing the loaded data frame or NULL if no data
#'         is loaded or if the data has been cleared.
#'
#' @details This server module provides functionality for:
#'   * Loading SPSS (.sav) files
#'   * Clearing loaded data
#'   * Error handling for file uploads
#'   * User notifications for successful/failed operations
#'
#' @importFrom shiny moduleServer reactiveVal observeEvent showNotification req
#' @importFrom shinyjs reset
#' @importFrom haven read_sav
#'
#' @seealso [dataLoaderUI()] for the corresponding UI component
#'
#' @export
dataLoaderServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Store the loaded data in a reactive value
    rv <- reactiveVal(NULL)

    # Handle the clear button click
    observeEvent(input$clear, {
      shinyjs::reset(session$ns("file"))
      rv(NULL)
      showNotification("Data has been cleared", type = "message")
    })

    # Handle file uploads
    observeEvent(input$file, {
      if (!is.null(input$file)) {
        tryCatch({
          data <- haven::read_sav(input$file$datapath)
          rv(data)
        },
        error = function(e) {
          showNotification(
            paste("Error reading the file:", e$message),
            type = "error"
          )
          rv(NULL)
        })
      }
    })

    return(rv)
  })
}
