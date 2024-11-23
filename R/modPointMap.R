#' The pointMapUI module to generate the user interface
#'
#' @param id namespace id
#' @import shiny
#' @import dplyr
#' @import sf
#' @import leaflet
#' @return A shiny module UI
#' @export
#' @examples
#' ui <- fluidPage(pointMapUI("pointmaptest"))
#'
#' server <- function(input, output, session) {
#'   pointMapServer("pointmaptest", df = reactive({ covid }))
#' }
#' shinyApp(ui, server)
pointMapUI <- function(id) {
  ns <- NS(id)
  ui <- tagList(
    tags$script(
      src = "https://cdn.jsdelivr.net/gh/Appsilon/shiny.tictoc@v0.2.0/shiny-tic-toc.min.js"
    ),
    selectInput(ns("cntry"), "Country", choices = NULL),
    selectInput(ns("st"), "State", choices = NULL),
    actionButton(ns("reset"), "Reset"),
    actionButton(ns("gobutton"), "Go!"),
    tableOutput(ns("data")),
    leafletOutput(ns("map"), height = "600px")
  )
}

#' The pointMapServer module to generate the server logic
#'
#' @param id namespace id
#' @param df data frame
#' @import shiny
#' @import dplyr
#' @import sf
#' @import leaflet
#' @return A shiny module server
#' @export
#' @examples
#' ui <- fluidPage(pointMapUI("pointmaptest"))
#'
#' server <- function(input, output, session) {
#'   pointMapServer("pointmaptest", df = reactive({ covid }))
#' }
#' shinyApp(ui, server)
pointMapServer <- function(id, df) {
  moduleServer(id, function(input, output, session) {
    # aggregated dataset

    agg_data <- reactive({
      df() |>
        group_by(country, state, lat_jittered, lon_jittered) |>
        summarise(
          total_deaths = sum(death, na.rm = TRUE),
          .groups = 'drop'
        )
    })

    # Initialize reactive values
    rv <- reactiveValues(
      current_data = NULL,
      needs_update = FALSE,
      reset_counter = 0  # Add a counter to track resets
    )

    # Reset functionality
    observeEvent(input$reset, {
      updateSelectInput(session, inputId = "cntry",
                        choices = sort(unique(agg_data()$country)))
      updateSelectInput(session, inputId = "st", choices = NULL)
      rv$current_data <- NULL
      rv$needs_update <- FALSE
      rv$reset_counter <- rv$reset_counter + 1  # Increment reset counter

      # Render map
      output$map <- renderLeaflet({
        # Create base map even if no data is selected yet
        leaflet() |>
          addTiles() |>
          setView(lng = 0, lat = 0, zoom = 2)
      })
    })

    # Initialize territory dropdown
    observeEvent(agg_data(), {
      choices <- sort(unique(agg_data()$country))
      updateSelectInput(session, inputId = "cntry", choices = choices)
    })

    # Update state dropdown when country changes
    observeEvent(input$cntry, {
      freezeReactiveValue(input, "st")
      filtered_data <- agg_data()[agg_data()$country == input$cntry, ]
      choices <- sort(unique(filtered_data$state))
      updateSelectInput(session, inputId = "st", choices = choices)
      rv$needs_update <- TRUE
    })

    # Update table only when Go button is clicked
    observeEvent(input$gobutton, {
      req(input$cntry, input$st)

      rv$current_data <- agg_data() |>
        filter(
          country == input$cntry,
          state == input$st
        ) |>
        select(country, state, total_deaths, lat_jittered, lon_jittered)

      rv$needs_update <- FALSE
    }, ignoreNULL = FALSE, ignoreInit = FALSE)  # Modified these parameters

    output$data <- renderTable({

      rv$reset_counter
      rv$current_data
    })

    # Render map
    output$map <- renderLeaflet({
      # Create base map even if no data is selected yet
      leaflet() |>
        addTiles() |>
        setView(lng = 0, lat = 0, zoom = 2)
    })

    # Update map markers when data changes
    observe({
      req(rv$current_data)

      leafletProxy("map", session) |>
        clearMarkers() |>
        fitBounds(
          lng1 = min(rv$current_data$lon_jittered),
          lat1 = min(rv$current_data$lat_jittered),
          lng2 = max(rv$current_data$lon_jittered),
          lat2 = max(rv$current_data$lat_jittered)
        ) |>
        addCircleMarkers(
          data = rv$current_data,
          lng = ~lon_jittered,
          lat = ~lat_jittered,
          # radius = ~sqrt(total_deaths) / 2,
          fillOpacity = 0.7,
          color = "red",
          stroke = FALSE,
          popup = ~paste(
            "<strong>Location:</strong>", state, ",", country, "<br>",
            "<strong>Total Deaths:</strong>", total_deaths
          )
        )
    })
  })
}




ui <- fluidPage(pointMapUI("pointmaptest"))

server <- function(input, output, session) {
  pointMapServer("pointmaptest", df = reactive({ covid }))
}

shinyApp(ui, server)
