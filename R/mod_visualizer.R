#' @import shiny
#' @import dplyr
#' @import ggplot2
NULL

#' Create UI for the Visualizer Module
#'
#' @description
#' This function creates the UI portion of a Shiny module that displays a histogram
#' of weight data. The UI consists of a tabset panel with a single "Plot" tab
#' containing a plot output.
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'   server function. The namespace for the module will be derived from this ID.
#'
#' @return A \code{shiny.tag.list} containing the UI elements for the module,
#'   specifically a tabset panel with a plot output.
#'
#' @examples
#' # In your app's UI:
#' ui <- fluidPage(
#'   titlePanel("Visualizer Module Test"),
#'   sidebarLayout(
#'     sidebarPanel(
#'       numericInput("sample_size", "Sample Size:", value = 100, min = 10, max = 1000),
#'       sliderInput("mean", "Mean Weight:", min = 0, max = 100, value = 50),
#'       sliderInput("sd", "Standard Deviation:", min = 1, max = 20, value = 10),
#'       actionButton("generate", "Generate Test Data")
#'     ),
#'     mainPanel(
#'       visualizerUI("viz1")
#'     )
#'   )
#' )
#'
#' @export
visualizerUI <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Plot",
               plotOutput(ns("plot")))
    )
  )
}

#' Server Function for the Visualizer Module
#'
#' @description
#' This function implements the server logic for a Shiny module that creates
#' a histogram visualization of weight data. It validates the input data and
#' generates a histogram using ggplot2.
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'   UI function.
#' @param processed_data A reactive expression that returns a data frame containing
#'   at least a 'weight' column. The weight values should be numeric or capable of
#'   being coerced to numeric.
#'
#' @details
#' The function performs several validation checks on the input data:
#' \itemize{
#'   \item Checks if data exists
#'   \item Verifies the data frame is not empty
#'   \item Confirms presence of 'weight' column
#' }
#'
#' The resulting plot is a histogram of the weight values using ggplot2.
#'
#' @examples
#' # In your app's server:
#' server <- function(input, output, session) {
#'   # Reactive value to store generated test data
#'   test_data <- reactiveVal()
#'
#'   # Generate test data when button is clicked
#'   observeEvent(input$generate, {
#'     data <- data.frame(
#'       weight = rnorm(input$sample_size,
#'                     mean = input$mean,
#'                     sd = input$sd)
#'     )
#'     test_data(data)
#'   })
#'
#'   # Call the visualizer module server
#'   visualizerServer("viz1", test_data)
#' }
#'
#' @import shiny
#' @import dplyr
#' @import ggplot2
#' @export
visualizerServer <- function(id, processed_data) {
  moduleServer(id, function(input, output, session) {
    # Generate plot
    output$plot <- renderPlot({
      req(processed_data())
      data <- processed_data()
      # Validate data structure
      validate(
        need(!is.null(data), "No data available"),
        need(nrow(data) > 0, "Data is empty"),
        need("weight" %in% names(data), "Data must contain 'weight' column")
      )
      ggplot2::ggplot(data, aes(x = as.numeric(weight))) +  ggplot2::geom_histogram()
    })
  })
}



# # app.R
# library(shiny)
# library(dplyr)
# library(ggplot2)
#
# # UI definition for the main app
# ui <- fluidPage(
#   titlePanel("Visualizer Module Test"),
#
#   sidebarLayout(
#     sidebarPanel(
#       numericInput("sample_size",
#                    "Sample Size:",
#                    value = 100,
#                    min = 10,
#                    max = 1000),
#
#       sliderInput("mean",
#                   "Mean Weight:",
#                   min = 0,
#                   max = 100,
#                   value = 50),
#
#       sliderInput("sd",
#                   "Standard Deviation:",
#                   min = 1,
#                   max = 20,
#                   value = 10),
#
#       actionButton("generate", "Generate Test Data")
#     ),
#
#     mainPanel(
#       # Your existing visualizer module UI
#       visualizerUI("viz1")
#     )
#   )
# )
#
# # Server logic
# server <- function(input, output, session) {
#   # Reactive value to store generated test data
#   test_data <- reactiveVal()
#
#   # Generate test data when button is clicked
#   observeEvent(input$generate, {
#     data <- data.frame(
#       weight = rnorm(input$sample_size,
#                      mean = input$mean,
#                      sd = input$sd)
#     )
#     test_data(data)
#   })
#
#   # Call your existing visualizer module server
#   visualizerServer("viz1", test_data)
# }
#
# # Run the app
# shinyApp(ui = ui, server = server)
