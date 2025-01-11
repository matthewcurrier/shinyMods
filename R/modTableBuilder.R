#' Create a user interface module for interacting with tabular data
#'
#' @param id the reference id for module
#' @return a UI module for tables
#' @export
#' @examples
#' \donttest{
#' modTblBuilderUI()
#' }
modTblBuilderUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$script(
      src = "https://cdn.jsdelivr.net/gh/Appsilon/shiny.tictoc@v0.2.0/shiny-tic-toc.min.js"
    ),
    selectInput(ns("dimensions"),
                "Select Dimensions:",
                choices = NULL,
                multiple = TRUE),
    selectInput(ns("metrics"),
                "Select Metrics:",
                choices = NULL,
                multiple = TRUE),
    actionButton(ns("reset"), "Reset"),
    actionButton(ns("gobutton"), "Submit"),
    textOutput(ns("message")),
    textOutput(ns("tbl_debug")),
    gt::gt_output(ns("data"))
  )
}
#' Create a server module for interacting with tabular data
#'
#' @param id the reference id for module
#' @param df a dataframe
#' @param filters stuff here
#' @return a UI module for tables
#' @export
#' @examples
#' \donttest{
#' modTblBuilderServer()
#' }


modTblBuilderServer <- function(id, df, filters){ #assuming df is a reactive!
  moduleServer(id, function(input, output, session) {
    stopifnot(is.reactive(df))

    rv <- reactiveValues(
      current_data = NULL,
      reset_counter = 0,
      show_message = TRUE,
      dims = filters[["dims"]],  # Store the column name here
      metrics = filters[["metrics"]]
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

      # df_col_names <- names(df())
      # d <- df_col_names %in% dims
      updateSelectInput(session,
                        inputId = "dimensions",
                        choices = rv$dims
                        )

      updateSelectInput(session,
                        inputId = "metrics",
                        choices = rv$metrics
      )


      rv$current_data <- NULL
      rv$reset_counter <- rv$reset_counter + 1
      rv$show_message <- TRUE
    })

    # Initialize dimensions dropdown
    observeEvent(df(), {
      choices <- rv$dims
      updateSelectInput(session,
                        inputId = "dimensions",
                        choices = choices
                        )
      rv$current_data <- NULL
      rv$show_message <- TRUE
    })

    # Initialize dimensions dropdown
    observeEvent(df(), {
      choices <- rv$metrics
      updateSelectInput(session,
                        inputId = "metrics",
                        choices = choices
                        )
      rv$current_data <- NULL
      rv$show_message <- TRUE
    })

    # Update table only when submit button is clicked
    observeEvent(input$gobutton, {
      req(input[["dimensions"]], input[["metrics"]])

      selected_cols <- c(input[["dimensions"]], input[["metrics"]])
      rv$current_data <- df() |>
        dplyr::select(dplyr::all_of(selected_cols)) |>
        as.data.frame()

      print(str(rv$current_data))



      rv$show_message <- FALSE
    }, ignoreNULL = FALSE, ignoreInit = FALSE)

    # Render table based on current_data
    output$data <- render_gt({
      req(rv$current_data)  # Only render when data exists
      # add some sort of check to make sure that there is only 1 item being rendered
      # adn that it is a table
      gt::gt(rv$current_data)

    })
  })
}



library(shiny)
library(gt)
myApp <- function() {
  ui <- fluidPage(
    modTblBuilderUI("tbl_small")
  )
  server <- function(input, output, session) {
    options(shiny.maxRequestSize=30*1024^2)

    modTblBuilderServer(
      "tbl_small",
      df = reactive({mtcars}),
      filters = list(
        dims = c("cyl", "vs", "am", "gear", "carb"),
        metrics = c("mpg", "disp", "wt", "qsec")
    )
    )
  }
  shinyApp(ui, server)
}

myApp()



my_metrics <- list(
  create_metric(
    "revenue",
    function(df) sum(df$revenue, na.rm = TRUE),
    "Total revenue"
  ),
  create_metric(
    "conversion_rate",
    function(df) sum(df$conversions, na.rm = TRUE) / sum(df$sessions, na.rm = TRUE),
    "Conversion rate"
  )
)

my_metrics <- list(
  "impressions" = create_metric(
    "impressions",
    function(df) sum(df$impr, na.rm = TRUE),
    "Total revenue"
  ),
  "cost_per_click" = create_metric(
    "cost_per_click",
    function(df) sum(df$pub_cost, na.rm = TRUE) / sum(df$clicks, na.rm = TRUE),
    "Cost Per Click"
  )
)

myApp2 <- function() {
  ui <- fluidPage(
    modTblBuilderUI("tbl_pss")
  )
  server <- function(input, output, session) {
    options(shiny.maxRequestSize=30*1024^2)

    modTblBuilderServer(
      "tbl_pss",
      df = reactive({pss}),
      filters = list(
        dims = c("date", "publisher", "region", "branding", "device"),
        metrics = names(my_metrics)
      )
    )
  }
  shinyApp(ui, server)
}

myApp2()



#
#
# Warning: Error in dplyr::select: ℹ In argument: `dplyr::all_of(selected_cols)`.
# Caused by error in `dplyr::all_of()`:
#   ! Can't subset elements that don't exist.
# ✖ Element `impressions` doesn't exist.
#   3: runApp
#   2: print.shiny.appobj
#   1: <Anonymous>
