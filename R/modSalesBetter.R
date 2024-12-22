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
    tags$script(
      src = "https://cdn.jsdelivr.net/gh/Appsilon/shiny.tictoc@v0.2.0/shiny-tic-toc.min.js"
    ),
    selectInput(ns("lev1"), "Territory", choices = NULL),
    selectInput(ns("cn"), "Customer", choices = NULL),
    selectInput(ns("on"), "Order number", choices = NULL),
    actionButton(ns("reset"), "Reset"),
    actionButton(ns("gobutton"), "Go!"),
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
salesServer <- function(id, df, cols_list) {
  moduleServer(id, function(input, output, session) {
    # Store level_1 in a reactive value to ensure proper reactivity
    rv <- reactiveValues(
      current_data = NULL,
      reset_counter = 0,
      show_message = TRUE,
      level_1 = cols_list[["level_1"]]  # Store the column name here
    )

    output$message <- renderText({
      if (rv$show_message) {
        "Make your selection and click submit to see the data."
      } else {
        ""
      }
    })

    # Reset functionality
    observeEvent(input$reset, {
      updateSelectInput(session, inputId = "lev1",
                        choices = sort(unique(df()[[rv$level_1]])))
      updateSelectInput(session, inputId = "cn", choices = NULL)
      updateSelectInput(session, inputId = "on", choices = NULL)
      rv$current_data <- NULL
      rv$reset_counter <- rv$reset_counter + 1
      rv$show_message <- TRUE
    })

    # Initialize territory dropdown
    observeEvent(df(), {
      choices <- sort(unique(df()[[rv$level_1]]))
      updateSelectInput(session, inputId = "lev1", choices = choices)
      rv$current_data <- NULL
      rv$show_message <- TRUE
    })

    # Update customer dropdown when territory changes
    observeEvent(input[["lev1"]], {
      freezeReactiveValue(input, "cn")
      filtered_data <- df()[df()[[rv$level_1]] == input[["lev1"]], ]
      choices <- sort(unique(filtered_data$CUSTOMERNAME))
      updateSelectInput(session, inputId = "cn", choices = choices)
      rv$current_data <- NULL
      rv$show_message <- TRUE
    })

    # Update order number dropdown when customer changes
    observeEvent(input$cn, {
      req(input[["lev1"]], input$cn)
      freezeReactiveValue(input, "on")
      filtered_data <- df()[df()[[rv$level_1]] == input[["lev1"]] &
                              df()$CUSTOMERNAME == input$cn, ]
      choices <- sort(unique(filtered_data$ORDERNUMBER))
      updateSelectInput(session, inputId = "on", choices = choices)
      rv$current_data <- NULL
      rv$show_message <- TRUE
    })

    # Update table only when Go button is clicked
    observeEvent(input$gobutton, {
      req(input[["lev1"]], input$cn, input$on)
      rv$current_data <- df() |>
        filter(
          !!sym(rv$level_1) == input[["lev1"]],  # Use !!sym() for dynamic filtering
          CUSTOMERNAME == input$cn,
          ORDERNUMBER == input$on
        ) |>
        select(QUANTITYORDERED, PRICEEACH, PRODUCTCODE)
      rv$show_message <- FALSE
    }, ignoreNULL = FALSE, ignoreInit = FALSE)

    # Render table based on current_data
    output$data <- renderTable({
      rv$reset_counter
      rv$current_data
    })
  })
}

# App definition
ui <- fluidPage(salesUI("sales1"))
server <- function(input, output, session) {
  salesServer("sales1", df = reactive({ sales }), list(level_1 = "TERRITORY"))
}
shinyApp(ui, server)
