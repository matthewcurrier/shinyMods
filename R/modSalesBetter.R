#' The salesUI module to generate code.
#'
#' @param id namespace id
#' @return A shiny module UI
#' @export
#' @examples
#' \donttest{
#' ui <- fluidPage(salesUI("sales1"))
#' server <- function(input, output, session) {
#'   salesServer("sales1",
#'               df = reactive({ sales }),
#'               dropdowns = list(
#'                 level_1 = "TERRITORY",
#'                 level_2 = "CUSTOMERNAME",
#'                 level_3 = "ORDERNUMBER")
#'               ,
#'               cols_for_table = c("TERRITORY", "CUSTOMERNAME", "ORDERNUMBER", "SALES")
#'               )
#' }
#' shinyApp(ui, server)
#' }


salesUI <- function(id) {
  ns <- NS(id)
  ui <- tagList(
    tags$script(
      src = "https://cdn.jsdelivr.net/gh/Appsilon/shiny.tictoc@v0.2.0/shiny-tic-toc.min.js"
    ),
    selectInput(ns("lev1"), "Territory", choices = NULL),
    selectInput(ns("lev2"), "Customer", choices = NULL),
    selectInput(ns("lev3"), "Order number", choices = NULL),
    actionButton(ns("reset"), "Reset"),
    actionButton(ns("gobutton"), "Go!"),
    textOutput(ns("message")),
    tableOutput(ns("data"))
  )
}

#' The salesServer module to generate code.
#' @param id namespace id
#' @param df a reactive dataframe with the data
#' @param dropdowns a list of three columns that will create the dropdown hiearchy
#' @param cols_for_table table columns to display in table
#' @return A shiny module Server
#' @export
#' @examples
#' \donttest{
#' ui <- fluidPage(salesUI("sales1"))
#' server <- function(input, output, session) {
#'   salesServer("sales1",
#'               df = reactive({ sales }),
#'               dropdowns = list(
#'                 level_1 = "TERRITORY",
#'                 level_2 = "CUSTOMERNAME",
#'                 level_3 = "ORDERNUMBER")
#'               ,
#'               cols_for_table = c("TERRITORY", "CUSTOMERNAME", "ORDERNUMBER", "SALES")
#'               )
#' }
#' shinyApp(ui, server)
#' ui <- fluidPage(salesUI("cars"))
#' server <- function(input, output, session) {
#'   salesServer("cars",
#'               df = reactive({ mtcars }),
#'               dropdowns = list(
#'                 level_1 = "cyl",
#'                 level_2 = "gear",
#'                 level_3 = "carb")
#'               ,
#'               cols_for_table = c("mpg", "cyl", "disp", "hp", "wt")
#'   )
#' }
#' shinyApp(ui, server)
#' }
#'


salesServer <- function(id, df, dropdowns, cols_for_table) {
  moduleServer(id, function(input, output, session) {
    # Store level_1 in a reactive value to ensure proper reactivity
    rv <- reactiveValues(
      current_data = NULL,
      reset_counter = 0,
      show_message = TRUE,
      level_1 = dropdowns[["level_1"]],  # Store the column name here
      level_2 = dropdowns[["level_2"]],
      level_3 = dropdowns[["level_3"]]
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
      # First, update the level 1 dropdown with all possible values
      updateSelectInput(session, inputId = "lev1",
                        choices = sort(unique(df()[[rv$level_1]])))

      # Then update level 2 based on the new level 1 selection
      filtered_data_l2 <- df()[df()[[rv$level_1]] == input$lev1, ]
      updateSelectInput(session, inputId = "lev2",
                        choices = sort(unique(filtered_data_l2[[rv$level_2]])))

      # Finally update level 3 based on both level 1 and 2 selections
      filtered_data_l3 <- filtered_data_l2[filtered_data_l2[[rv$level_2]] == input$lev2, ]
      updateSelectInput(session, inputId = "lev3",
                        choices = sort(unique(filtered_data_l3[[rv$level_3]])))
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
      freezeReactiveValue(input, "lev2")
      filtered_data <- df()[df()[[rv$level_1]] == input[["lev1"]], ]
      choices <- sort(unique(filtered_data[[rv$level_2]]))
      updateSelectInput(session, inputId = "lev2", choices = choices)
      rv$current_data <- NULL
      rv$show_message <- TRUE
    })

    # Update order number dropdown when customer changes
    observeEvent(input[["lev2"]], {
      req(input[["lev1"]], input[["lev2"]])
      freezeReactiveValue(input, "lev3")
      filtered_data <- df()[df()[[rv$level_1]] == input[["lev1"]] &
                              df()[[rv$level_2]] == input[["lev2"]], ]
      choices <- sort(unique(filtered_data[[rv$level_3]]))
      updateSelectInput(session, inputId = "lev3", choices = choices)
      rv$current_data <- NULL
      rv$show_message <- TRUE
    })

    # Update order number dropdown when customer changes
    observeEvent(input[["lev3"]], {
      req(input[["lev1"]], input[["lev2"]], input[["lev3"]])
      filtered_data <- df()[df()[[rv$level_1]] == input[["lev1"]] &
                              df()[[rv$level_2]] == input[["lev2"]] &
                              df()[[rv$level_3]] == input[["lev3"]], ]
      # choices <- sort(unique(filtered_data[[rv$level_3]]))
      # updateSelectInput(session, inputId = "lev3", choices = choices)
      rv$current_data <- NULL
      rv$show_message <- TRUE
    })

    # Update table only when Go button is clicked
    observeEvent(input$gobutton, {
      req(input[["lev1"]], input[["lev2"]], input[["lev3"]])
      rv$current_data <- df() |>
        filter(
          !!sym(rv$level_1) == input[["lev1"]],
          !!sym(rv$level_2) == input[["lev2"]],
          !!sym(rv$level_3) == input[["lev3"]]
        ) |>
        select(any_of(cols_for_table))
      rv$show_message <- FALSE
    }, ignoreNULL = FALSE, ignoreInit = FALSE)

    # Render table based on current_data
    output$data <- renderTable({
      rv$reset_counter
      rv$current_data
    })
  })
}
