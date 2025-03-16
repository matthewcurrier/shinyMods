# UI Module
#' @export
datawrapper_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fileInput(ns("data"), "Upload CSV file", accept = c(".csv")),
    selectInput(ns("chart_type"), "Chart Type",
                choices = c("d3-bars", "tables", "d3-scatter-plot")),
    uiOutput(ns("column_selectors")),
    textInput(ns("title"), "Chart Title", "My Chart"),
    actionButton(ns("create"), "Create Chart", class = "btn-primary"),
    verbatimTextOutput(ns("debug")),  # Added debug output
    uiOutput(ns("chart_url"))
  )
}

# Server Module
#' @export
datawrapper_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Reactive values
    data <- reactiveVal(NULL)
    chart_id <- reactiveVal(NULL)
    debug_message <- reactiveVal("")
    chart_url <- reactiveVal("")
    iframe_code <- reactiveVal("")

    # Read uploaded data
    observeEvent(input$data, {
      req(input$data)
      tryCatch({
        df <- read.csv(input$data$datapath)
        data(df)
        debug_message("Data loaded successfully")
      }, error = function(e) {
        debug_message(paste("Error loading data:", e$message))
      })
    })

    # Dynamic column selectors
    output$column_selectors <- renderUI({
      req(data())
      ns <- session$ns

      tagList(
        selectInput(ns("x_col"), "X-axis Column",
                    choices = names(data())),
        selectInput(ns("y_col"), "Y-axis Column",
                    choices = names(data()))
      )
    })

    # Create chart when button is clicked
    observeEvent(input$create, {
      req(data(), input$x_col, input$y_col, input$chart_type)

      tryCatch({
        # Prepare data for the chart
        chart_data <- data()[, c(input$x_col, input$y_col)]
        debug_message(paste("Data prepared:", paste(names(chart_data), collapse=", ")))

        # Create new chart
        new_chart_id <- dw_create_chart(
          title = input$title,
          type = input$chart_type
        )
        debug_message(paste("Chart created with ID:", new_chart_id$content$publicId))
        chart_id(new_chart_id$content$publicId)

        # Add data to the chart
        dw_data_to_chart(chart_data, chart_id())
        debug_message("Data added to chart successfully")

        # Set chart properties
        dw_edit_chart(
          chart_id(),
          title = input$title
        )
        debug_message("Chart properties updated")

        # Publish the chart
        publish_result <- dw_publish_chart(chart_id())
        debug_message("Chart published successfully")

        # Store the URL and iframe code
        url <- paste0("https://datawrapper.dwcdn.net/", chart_id(), "/1/")
        chart_url(url)

        iframe <- sprintf(
          '<iframe title="%s" aria-label="Chart" id="datawrapper-chart-%s" src="%s" scrolling="no" frameborder="0" style="width: 0; min-width: 100%% !important; border: none;" height="400"></iframe>',
          input$title,
          chart_id(),
          url
        )
        iframe_code(iframe)

      }, error = function(e) {
        debug_message(paste("Error in chart creation process:", e$message))
      })
    })

    # Debug output
    output$debug <- renderPrint({
      debug_message()
    })

    # Chart URL output
    output$chart_url <- renderText({
      if (!is.null(chart_url())) {
        paste("Chart URL:", chart_url())
      }
    })

    # Chart embed output
    output$chart_embed <- renderUI({
      if (!is.null(iframe_code())) {
        HTML(iframe_code())
      }
    })
  })
}

library(shiny)
library(bslib)
library(DatawRappr)

# Main application
# Main application
ui <- page_sidebar(
  title = "Datawrapper Chart Creator",
  sidebar = div(
    style = "background-color: #f0f0f0; font-size: 0.9em;",
    datawrapper_ui("dw1")
  ),
  theme = bs_theme(version = 5) |>
    bs_add_rules(
      "
      .sidebar {
        background-color: #f0f0f0 !important;
      }
      .sidebar .form-control {
        font-size: 0.9em;
      }
      .sidebar .btn {
        font-size: 0.9em;
      }
      .sidebar label {
        font-size: .6em;
      }
      "
    ),
  card(
    htmlOutput("dw1-chart_embed"),
    verbatimTextOutput("dw1-chart_url")
  )
)

server <- function(input, output, session) {
  datawrapper_server("dw1")
}

shinyApp(ui, server)
