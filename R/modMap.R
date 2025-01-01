#' County Map Module UI
#'
#' @description Creates the UI portion of the county map module for displaying
#'   demographic data choropleth visualizations.
#'
#' @param id The module ID used to create a namespace
#'
#' @return A Shiny UI element containing the map interface
#' @import shiny
#' @import leaflet
#' @import sf
#' @export
#' @examples
#' library(shiny)
#' ui <- fluidPage(mapUI("map1"))
#'
#' server <- function(input, output, session) {
#'
#'  # mydata <- readRDS(here("data", "county_eth.RDS"))
#'  mapServer("map1", df = reactive({ county_pop }) )
#'
#'  }
#' shinyApp(ui, server)

mapUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("ethnicity"),
                "Select Ethnicity:",
                choices = c(
                  "Total" = "Total",
                  "White" = "White",
                  "Black" = "Black"
                )),
    leafletOutput(ns("county_map"), height = "800px")
  )
}


mapServer <- function(id, df) {
  moduleServer(id, function(input, output, session) {
    # Reactive value for census data
    census_data <- reactive({
      df() |>
        dplyr::filter(variable==input$ethnicity)
    })

    # Render the map
    output$county_map <- renderLeaflet({

        # st_transform(4326)

      leaflet(census_data()) |>
        addProviderTiles(providers$CartoDB.Positron) |>
        setView(lng = -96.25, lat = 39.50, zoom = 4)
        # Set max bounds to prevent panning away from US
        # setMaxBounds(
        #   lng1 = -125, # West coast
        #   lat1 = 24,   # Southern border
        #   lng2 = -67,  # East coast
        #   lat2 = 50    # Northern border
        # )
    })


    observe({

      pal <- colorNumeric(
        palette = "viridis",
        domain = census_data()$value
      )

      leafletProxy("county_map", data = census_data()) |>
        clearShapes() |>
        addPolygons(
          fillColor = ~pal(value),
          weight = 0.5,
          opacity = 1,
          color = "white",
          fillOpacity = 0.7,
          popup = ~paste(
            "<b>", name, "</b><br/>",
            "value: ", formatC(value, big.mark = ",")
          ),
          highlightOptions = highlightOptions(
            weight = 2,
            color = "#666",
            fillOpacity = 0.7,
            bringToFront = TRUE
          )
        ) |>
        addLegend(
          position = "bottomright",
          pal = pal,
          values = ~value,
          title = paste(stringr::str_to_title(input$ethnicity), "value"),
          labFormat = labelFormat(big.mark = ",")
        )
    })
  })
}
