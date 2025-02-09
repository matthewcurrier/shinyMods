#' Data loader UI module
#' @param id Module ID
#' @export
data_loader_ui <- function(id) {
  ns <- NS(id)

  list(
  data_loader = tagList(
    # Combine file_loader and feedback into a single tagList
    uiOutput(ns("file_upload")),
    actionButton(ns("clear"), "Clear Data")
  ),
  feedback = tagList(
    # Combine file_loader and feedback into a single tagList
    verbatimTextOutput(ns("dataInfo")),  # Added ns() wrapper
    tableOutput(ns("dataPreview"))       # Added ns() wrapper
  )
  )
}

#' Data loader server module
#' @param id Module ID
#' @export
data_loader_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Initialize reactive values
    data <- reactiveVal(NULL)
    fileReset <- reactiveVal(0)

    # Render file input
    output$file_upload <- renderUI({
      fileReset()  # Dependency for re-render
      fileInput(session$ns("file"), "Upload SAV file", accept = ".sav")
    })

    # Handle file uploads
    observeEvent(input$file, {
      req(input$file)

      tryCatch({
        new_data <- haven::read_sav(input$file$datapath)
        required_cols <- c("AGE", "AGECAT", "PARTYLN")

        if (!all(required_cols %in% names(new_data))) {
          missing_cols <- setdiff(required_cols, names(new_data))
          showNotification(
            sprintf("Dataset not loaded. Missing required columns: %s", paste(missing_cols, collapse = ", ")),
            type = "error",
            duration = 7
          )
          return()
        }

        data(new_data)
        showNotification("Data loaded successfully", type = "message")
      }, error = function(e) {
        showNotification(
          sprintf("Error reading file: %s", e$message),
          type = "error",
          duration = 7
        )
      })
    })

    # Handle clear button
    observeEvent(input$clear, {
      data(NULL)
      fileReset(fileReset() + 1)
      showNotification("Data has been cleared", type = "message")
    })

    # Update data preview and info
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

    # Return validated data
    return(reactive({
      current_data <- data()
      validate(
        need(current_data, "Please upload a data file."),
        need(all(c("AGE", "AGECAT", "PARTYLN") %in% names(current_data)),
             "Missing required columns")
      )
      current_data
    }))
  })
}




library(shiny)
library(shinyjs)
library(haven)

# Source your module file first
# source("data_loader.R")  # Uncomment this line when using

# UI
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Data Loader Test"),

  sidebarLayout(
    sidebarPanel(
      # Use the data loader UI
      data_loader_ui("test_loader")$data_loader
    ),

    mainPanel(
      # Add a reactive output to show when data is loaded
      h4("Data Status:"),
      # verbatimTextOutput("dataStatus"),
      data_loader_ui("test_loader")$feedback
      # Add a plot to verify the data is accessible
      # h4("Age Distribution (when data is loaded):"),
      # plotOutput("agePlot")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Initialize the data loader
  loaded_data <- data_loader_server("test_loader")

  # Show data status
  # output$dataStatus <- renderPrint({
  #   if (is.null(loaded_data())) {
  #     cat("No data loaded")
  #   } else {
  #     cat("Data successfully loaded\n")
  #     cat("Number of rows:", nrow(loaded_data()), "\n")
  #     cat("Number of unique age categories:", length(unique(loaded_data()$AGECAT)))
  #   }
  # })

  # Create a simple plot using the loaded data
  output$agePlot <- renderPlot({
    req(loaded_data())
    hist(loaded_data()$AGE,
         main = "Age Distribution",
         xlab = "Age",
         col = "lightblue",
         border = "white")
  })
}

# Run the app
shinyApp(ui = ui, server = server)

# Test data creation (uncomment and run separately to create test data)
#test_data <- data.frame(
#  AGE = sample(18:90, 100, replace = TRUE),
#  AGECAT = sample(1:5, 100, replace = TRUE),
#  PARTYLN = sample(1:3, 100, replace = TRUE)
#)
#haven::write_sav(test_data, "test_data.sav")



# https://claude.ai/chat/17e47b34-30be-4201-a5cc-4db0f1d90eca
#
# I'll explain how the fileReset reactive works in this Shiny module. It's an interesting mechanism for resetting the file input widget, so let me break it down and create a diagram to visualize the flow.
#
#
#
# Here's how the fileReset reactive mechanism works:
#
# 1. Initial Setup:
#    - `fileReset <- reactiveVal(0)` creates a reactive value initialized to 0
#    - The file input UI is rendered using `renderUI` with a dependency on fileReset
#
# 2. The Key Components:
#    ```r
#    # Initialize the reactive value
#    fileReset <- reactiveVal(0)
#
#    # Render file input with dependency
#    output$file_upload <- renderUI({
#      fileReset()  # Dependency for re-render
#      fileInput(session$ns("file"), "Upload SAV file", accept = ".sav")
#    })
#    ```
#
# 3. The Reset Process:
#    ```r
#    observeEvent(input$clear, {
#      data(NULL)  # Clear the data
#      fileReset(fileReset() + 1)  # Increment the counter
#      showNotification("Data has been cleared", type = "message")
#    })
#    ```
#
# The clever part about this implementation is that it solves a common problem in Shiny: the file input widget maintains its state even after you clear the associated data. Here's why this solution works:
#
#   1. When the clear button is clicked, `fileReset()` is incremented
# 2. Since `fileReset()` is a dependency in the `renderUI`, any change to its value triggers a complete re-render of the file input
# 3. The re-render creates a completely new file input widget, effectively clearing any previous file selection
#
# This is particularly useful because normally, just clearing the data wouldn't reset the file input widget's state. The user would still see the old filename, which could be confusing. By forcing a re-render of the entire widget, the UI stays consistent with the underlying data state.
#
# Would you like me to elaborate on any particular aspect of this mechanism?
