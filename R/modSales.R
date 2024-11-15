#' The salesUI module to generate code.
#'
#' @param id namespace id
#' @import shiny
#' @import dplyr
#' @return A shiny module UI
#' @export
salesUI <- function(id) {
  ns <- NS(id)
  ui <- tagList(
    selectInput(ns("terr"), "Territory", choices = NULL),
    selectInput(ns("cn"), "Customer", choices = NULL),
    selectInput(ns("on"), "Order number", choices = NULL),
    actionButton(ns("reset"), "Reset"),
    actionButton(ns("gobutton"), "Go!"),
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
salesServer <- function(id, df) {
  moduleServer(id, function(input, output, session) {
    # Initialize reactive values
    rv <- reactiveValues(
      current_data = NULL,
      needs_update = FALSE,
      reset_counter = 0  # Add a counter to track resets
    )

    # Reset functionality
    observeEvent(input$reset, {
      updateSelectInput(session, inputId = "terr",
                        choices = sort(unique(df()$TERRITORY)))
      updateSelectInput(session, inputId = "cn", choices = NULL)
      updateSelectInput(session, inputId = "on", choices = NULL)
      rv$current_data <- NULL
      rv$needs_update <- FALSE
      rv$reset_counter <- rv$reset_counter + 1  # Increment reset counter
    })

    # Initialize territory dropdown
    observeEvent(df(), {
      choices <- sort(unique(df()$TERRITORY))
      updateSelectInput(session, inputId = "terr", choices = choices)
    })

    # Update customer dropdown when territory changes
    observeEvent(input$terr, {
      freezeReactiveValue(input, "cn")
      filtered_data <- df()[df()$TERRITORY == input$terr, ]
      choices <- sort(unique(filtered_data$CUSTOMERNAME))
      updateSelectInput(session, inputId = "cn", choices = choices)
      rv$needs_update <- TRUE
    })

    # Update order number dropdown when customer changes
    observeEvent(input$cn, {
      req(input$terr, input$cn)
      freezeReactiveValue(input, "on")
      filtered_data <- df()[df()$TERRITORY == input$terr &
                              df()$CUSTOMERNAME == input$cn, ]
      choices <- sort(unique(filtered_data$ORDERNUMBER))
      updateSelectInput(session, inputId = "on", choices = choices)
      rv$needs_update <- TRUE
    })

    # Update table only when Go button is clicked
    observeEvent(input$gobutton, {
      req(input$terr, input$cn, input$on)

      rv$current_data <- df() %>%
        filter(
          TERRITORY == input$terr,
          CUSTOMERNAME == input$cn,
          ORDERNUMBER == input$on
        ) %>%
        select(QUANTITYORDERED, PRICEEACH, PRODUCTCODE)

      rv$needs_update <- FALSE
    }, ignoreNULL = FALSE, ignoreInit = FALSE)  # Modified these parameters

    # Render table based on current_data
    output$data <- renderTable({
      # Observe reset_counter to ensure reactivity after reset
      rv$reset_counter
      rv$current_data
    })
  })
}

ui <- fluidPage(salesUI("sales1"))

server <- function(input, output, session) {
  salesServer("sales1", df = reactive({ sales }))
}

shinyApp(ui, server)
