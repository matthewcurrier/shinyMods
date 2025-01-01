#' The leafletMapUI module to generate the user interface
#'
#' @param id namespace id
#' @import sf
#' @import leaflet
#' @return A shiny module UI
#' @export
#' @examples
#' library(shiny)
#' ui <- fluidPage(pointMapUI("pointmaptest"))
#'
#' server <- function(input, output, session) {
#'   pointMapServer("pointmaptest", df = reactive({ covid }))
#' }
#' shinyApp(ui, server)
leafletMapUI <- function(id) {
  ns <- NS(id)
  ui <- tagList(
    tags$script(
      src = "https://cdn.jsdelivr.net/gh/Appsilon/shiny.tictoc@v0.2.0/shiny-tic-toc.min.js"
    ),
    # selectInput(ns("cntry"), "Country", choices = NULL),
    # selectInput(ns("st"), "State", choices = NULL),
    # actionButton(ns("reset"), "Reset"),
    # actionButton(ns("gobutton"), "Go!"),
    # tableOutput(ns("data")),
    leafletOutput(ns("map"), height = "600px")
  )
}

#' The leafletMapServer module to generate the map and interactivity
#' @param id namespace id
#' @param df data frame
#' @import sf
#' @import leaflet
#' @return A shiny module server
#' @export
#' @examples
#' library(shiny)
#' ui <- fluidPage(pointMapUI("pointmaptest"))
#'
#' server <- function(input, output, session) {
#'   pointMapServer("pointmaptest", df = reactive({ covid }))
#' }
#' shinyApp(ui, server)
leafletMapServer <- function(id, df) {
  moduleServer(id, function(input, output, session) {
})
}
