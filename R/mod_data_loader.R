#' Data loader UI module
#' @param id Module ID
#' @export
data_loader_ui <- function(id) {
  ns <- NS(id)

  list(
    data_loader = tagList(
      uiOutput(ns("file_upload")),
      actionButton(ns("clear"), "Clear Data")
    ),
    feedback = tagList(
      verbatimTextOutput(ns("dataInfo")),
      tableOutput(ns("dataPreview"))
    )
  )
}

#' Data loader server module
#' @param id Module ID
#' @export
data_loader_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    required_cols <- c("AGE", "AGECAT", "PARTYLN")

    data <- reactiveVal(NULL)
    file_reset <- reactiveVal(0)


    output$file_upload <- renderUI({
      file_reset() # Trigger rerender of fileInput button when 'clear' is hit
      fileInput(session$ns("file"), "Upload SAV file", accept = ".sav")
    })

    observeEvent(input$file, {
      req(input$file)

      tryCatch(
        {
          new_data <- haven::read_sav(input$file$datapath)


          if (!all(required_cols %in% names(new_data))) {
            missing_cols <- setdiff(required_cols, names(new_data))
            showNotification(
              sprintf(
                "Dataset not loaded. Missing required columns: %s",
                paste(missing_cols, collapse = ", ")
              ),
              type = "error",
              duration = 7
            )
            return()
          }

          data(new_data)
          showNotification("Data loaded successfully", type = "message")
        },
        error = function(e) {
          showNotification(
            sprintf("Error reading file: %s", e$message),
            type = "error",
            duration = 7
          )
        }
      )
    })

    # Handle clear button
    observeEvent(input$clear, {
      data(NULL)
      file_reset(file_reset() + 1)
      showNotification("Data has been cleared", type = "message")
    })

    observe({
      current_data <- data()

      output$dataInfo <- renderPrint({
        req(current_data)
        cat(sprintf(
          "Dataset Information:\nRows: %d\nColumns: %d\n\nColumn names:\n%s",
          nrow(current_data),
          ncol(current_data),
          paste(names(current_data), collapse = ", ")
        ))
      })

      output$dataPreview <- renderTable({
        req(current_data)
        head(current_data, 5)
      })
    })


    return(reactive({
      current_data <- data()
      validate(
        need(current_data, "Please upload a data file."),
        need(
          all(c("AGE", "AGECAT", "PARTYLN") %in% names(current_data)),
          "Missing required columns"
        )
      )
      current_data
    }))
  })
}




library(shiny)
library(shinyjs)
library(haven)


# UI
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Data Loader Test"),
  sidebarLayout(
    sidebarPanel(
      data_loader_ui("test_loader")$data_loader
    ),
    mainPanel(
      h4("Data Status:"),
      data_loader_ui("test_loader")$feedback
    )
  )
)

# Server
server <- function(input, output, session) {
  loaded_data <- data_loader_server("test_loader")


  output$agePlot <- renderPlot({
    req(loaded_data())
    hist(loaded_data()$AGE,
      main = "Age Distribution",
      xlab = "Age",
      col = "lightblue",
      border = "white"
    )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
