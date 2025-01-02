#' The salesUI module to generate code.
#'
#' @param id namespace id
#' @import shiny
#' @import dplyr
#' @return A shiny module UI
#' @export
#' @examples
#' \donttest{
#' ui <- fluidPage(salesUI("sales1"))
#' server <- function(input, output, session) {
#'   salesServer("sales1", df = reactive({ sales }))
#' }
#' shinyApp(ui, server)
#' }
salesUI <- function(id) {
  ns <- NS(id)
  ui <- tagList(
    tags$script(
      src = "https://cdn.jsdelivr.net/gh/Appsilon/shiny.tictoc@v0.2.0/shiny-tic-toc.min.js"
    ),
    selectInput(ns("terr"), "Territory", choices = NULL),
    selectInput(ns("cn"), "Customer", choices = NULL),
    selectInput(ns("on"), "Order number", choices = NULL),
    actionButton(ns("reset"), "Reset"),
    actionButton(ns("gobutton"), "Go!"),
    # Add textOutput for the message
    textOutput(ns("message")),
    tableOutput(ns("data"))
  )
}

#' The salesServer module to generate code.
#' @param id namespace id
#' @param df a reactive dataframe with the data
#' @import shiny
#' @import dplyr
#' @return A shiny module Server
#' @export
#' @examples
#' \donttest{
#' ui <- fluidPage(salesUI("sales1"))
#' server <- function(input, output, session) {
#'   salesServer("sales1", df = reactive({ sales }))
#' }
#' shinyApp(ui, server)
#' }
salesServer <- function(id, df) {
  moduleServer(id, function(input, output, session) {
    # Initialize reactive values
    rv <- reactiveValues(
      current_data = NULL,
      reset_counter = 0,  # Add a counter to track resets
      show_message = TRUE  # Add flag for showing/hiding message
    )

    # Default message
    output$message <- renderText({
      if (rv$show_message) {
        "Make your selection and click submit to see the data."
      } else {
        ""
      }
    })

    # Reset functionality
    observeEvent(input$reset, {
      updateSelectInput(session, inputId = "terr",
                        choices = sort(unique(df()$TERRITORY)))
      updateSelectInput(session, inputId = "cn", choices = NULL)
      updateSelectInput(session, inputId = "on", choices = NULL)
      rv$current_data <- NULL
      rv$reset_counter <- rv$reset_counter + 1  # Increment reset counter
      rv$show_message <- TRUE  # Show message on reset
    })

    # Initialize territory dropdown
    observeEvent(df(), {
      choices <- sort(unique(df()$TERRITORY))
      updateSelectInput(session, inputId = "terr", choices = choices)
      rv$current_data <- NULL
      rv$show_message <- TRUE

    })

    # Update customer dropdown when territory changes
    observeEvent(input$terr, {
      freezeReactiveValue(input, "cn")
      filtered_data <- df()[df()$TERRITORY == input$terr, ]
      choices <- sort(unique(filtered_data$CUSTOMERNAME))
      updateSelectInput(session, inputId = "cn", choices = choices)
      rv$current_data <- NULL
      rv$show_message <- TRUE
    })

    # Update order number dropdown when customer changes
    observeEvent(input$cn, {
      req(input$terr, input$cn)
      freezeReactiveValue(input, "on")
      filtered_data <- df()[df()$TERRITORY == input$terr &
                              df()$CUSTOMERNAME == input$cn, ]
      choices <- sort(unique(filtered_data$ORDERNUMBER))
      updateSelectInput(session, inputId = "on", choices = choices)
      rv$current_data <- NULL
      rv$show_message <- TRUE
    })

    # Update table only when Go button is clicked
    observeEvent(input$gobutton, {
      req(input$terr, input$cn, input$on)
      rv$current_data <- df() |>
        filter(
          TERRITORY == input$terr,
          CUSTOMERNAME == input$cn,
          ORDERNUMBER == input$on
        ) |>
        select(QUANTITYORDERED, PRICEEACH, PRODUCTCODE)
      rv$show_message <- FALSE  # Hide message when showing data
    }, ignoreNULL = FALSE, ignoreInit = FALSE)

    # Render table based on current_data
    output$data <- renderTable({
      # Observe reset_counter to ensure reactivity after reset
      rv$reset_counter
      rv$current_data
    })
  })
}

