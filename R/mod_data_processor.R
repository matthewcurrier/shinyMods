library(shiny)
library(dplyr)
library(forcats)
library(haven)

# Module UI function
dataProcessorUI <- function(id) {
  ns <- NS(id)
  tagList(
    verbatimTextOutput(ns("structure")),
    tableOutput(ns("preview"))
  )
}

# Fixed Module Server function
dataProcessorServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    print(paste("at top data is a reactive?", is.reactive(data)))

    # Reactive value to store the cleaned dataframe
    cleaned_df <- reactiveVal(NULL)

    # Function to convert factors to strings using dplyr
    clean_data <- function(data) {
      data |>
        mutate(
          across(where(is.labelled), ~as.character(as_factor(.))),  # Convert labelled to character via factor
          across(where(is.factor), as.character)  # Convert any remaining factors to character
        ) |>
        janitor::clean_names() |>
        as_tibble()
    }

    # Observer for the data changes
    observe({
      req(data())
      # Clean the data
      cleaned <- clean_data(data())
      # Store in reactive value
      cleaned_df(cleaned)
      # Show success message
      showNotification(
        sprintf("Cleaned %d columns in the dataset!", ncol(cleaned)),
        type = "message"
      )
    })

    print(paste("at bottom cleaned_df is a reactive?", is.reactive(cleaned_df)))
    return(cleaned_df)
  })
}

# # Main UI
# ui <- fluidPage(
#   titlePanel("MTCars Data Processor Demo"),
#
#   fluidRow(
#     column(6,
#            h3("Original Data"),
#            verbatimTextOutput("original_structure"),
#            tableOutput("original_preview")
#     ),
#     column(6,
#            h3("Cleaned Data"),
#            dataProcessorUI("test")
#     )
#   )
# )
#
# # Server using modified mtcars
# server <- function(input, output, session) {
#   # Modify mtcars to include factors
#   modified_mtcars <- reactiveVal({
#     df <- mtcars
#     # Convert cyl to factor
#     df$cyl <- factor(df$cyl, labels = c("Four", "Six", "Eight"))
#     # Convert am to factor
#     df$am <- factor(df$am, labels = c("Automatic", "Manual"))
#     df
#   })
#
#   # Display original data structure
#   output$original_structure <- renderPrint({
#     str(modified_mtcars())
#   })
#
#   # Display original data preview
#   output$original_preview <- renderTable({
#     head(modified_mtcars())
#   })
#
#   # Use the module
#   result <- dataProcessorServer("test", modified_mtcars)
#
#   # Add outputs for the cleaned data
#   moduleServer("test", function(input, output, session) {
#     output$structure <- renderPrint({
#       req(result())
#       str(result())
#     })
#
#     output$preview <- renderTable({
#       req(result())
#       head(result())
#     })
#   })
#
#   # Observer to check results (keeping console output for debugging)
#   observe({
#     req(result())
#     print("Original mtcars data types:")
#     print(sapply(modified_mtcars(), class))
#     print("Cleaned mtcars data types:")
#     print(sapply(result(), class))
#   })
# }
#
# # Run the app
# shinyApp(ui = ui, server = server)
