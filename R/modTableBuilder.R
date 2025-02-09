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
#' @param summary_fn your summary with metrics
#' @return a UI module for tables
#' @export
#' @examples
#' \donttest{
#' modTblBuilderServer()
#' }


modTblBuilderServer <- function(id, df, filters, summary_fn){ #assuming df is a reactive!
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

      # selected_cols <- c(input[["dimensions"]], input[["metrics"]])
      # Apply the summary function with current selections
      rv$current_data <- df() |>
        group_by(across(all_of(input$dimensions))) |>
        summarise(
          !!!summary_fn
        ) |>
        ungroup() |>
        select(all_of(c(input$dimensions, input$metrics))) |>


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
library(dplyr)
# my_metrics <- function(df, ...) {
#   # test if is dataframe
#   # test if specific columns are present: impressions, clicks, spend, pageviews
#   # What should be done with missing values? Then test is treatment for NA
#   # values is correct
#   summarise(
#     df,
#     impressions = sum(impressions, na.rm = TRUE),
#     clicks = sum(clicks, na.rm = TRUE),
#     spend = sum(spend, na.rm = TRUE),
#     cpc = sum(spend, na.rm = TRUE) / sum(clicks, na.rm = TRUE),
#     cpm = sum(spend, na.rm = TRUE) / (sum(impressions, na.rm = TRUE) / 1000),
#     ctr = sum(clicks, na.rm = TRUE) / sum(impressions, na.rm = TRUE)
#   )
# }


sales_calculations <- list(
  impressions = quo( sum(.data$impressions, na.rm = TRUE)),
  clicks = quo( sum(.data$clicks, na.rm = TRUE)),
  spend = quo( sum(.data$spend, na.rm = TRUE)),
  count = quo( n()),
  cpc = quo( sum(.data$spend, na.rm = TRUE) / sum(.data$clicks, na.rm = TRUE)),
  cpm = quo( sum(.data$spend, na.rm = TRUE) / (sum(.data$impressions, na.rm = TRUE) / 1000)),
  ctr = quo( sum(.data$clicks, na.rm = TRUE) / sum(.data$impressions, na.rm = TRUE))
)

myApp2 <- function() {
  ui <- fluidPage(
    modTblBuilderUI("tbl_pss")
  )
  server <- function(input, output, session) {
    options(shiny.maxRequestSize=30*1024^2)

    pss2 <- pss |>
      rename(
        impressions = impr,
        spend = pub_cost
      )

    modTblBuilderServer(
      "tbl_pss",
      df = reactive({pss2}),
      filters = list(
        dims = c("date", "publisher", "region", "branding", "device"),
        metrics = c("impressions", "clicks", "spend",
                    "cpc", "cpm", "ctr", "count")
      ),
      summary_fn = sales_calculations
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
