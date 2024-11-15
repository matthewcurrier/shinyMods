#' The salesUI module to generate code.
#'
#' @param id namespace id
#' @import shiny
#' @import dplyr
#' @return A shiny module UI
#' @export
#'
#' @examples
#' salesUI("sales1")
#'
#' ui <- fluidPage(salesUI("sales1"))
#'
#' server <- function(input, output, session) {
#'
#'   salesServer("sales1", df = reactive({ sales }) )
#'
#' }
#'
#' shinyApp(ui, server)


salesUI <- function(id) {
  ns <- NS(id)

  ui <- tagList(
    selectInput(ns("terr"), "Territory", choices = NULL),
    selectInput(ns("cn"), "Customer", choices = NULL),
    selectInput(ns("on"), "Order number", choices = NULL),
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
#'
#' @examples
#' salesServer("sales1")

salesServer <- function(id, df) {
  moduleServer(id, function(input, output, session) {

    observeEvent(df(), {
      choices <- unique(df()$TERRITORY)
      updateSelectInput(session, inputId = "terr", choices = choices)
    })
    territory <- reactive({
      req(df())
      df() |>
        filter(TERRITORY == input$terr)
    })
    observeEvent(territory(), {
      freezeReactiveValue(input, "cn")
      choices <- sort(unique(territory()$CUSTOMERNAME))
      updateSelectInput(session, inputId = "cn", choices = choices)
    })

    customer <- reactive({
      req(input$cn)
      filter(territory(), CUSTOMERNAME == input$cn)
    })
    observeEvent(customer(), {
      freezeReactiveValue(input, "on")
      choices <- sort(unique(customer()$ORDERNUMBER))
      updateSelectInput(session, inputId = "on", choices = choices)
    })

    output$data <- renderTable({
      req(input$on)
      customer() |>
        filter(ORDERNUMBER == input$on) %>%
        select(QUANTITYORDERED, PRICEEACH, PRODUCTCODE)
    }) |> bindEvent(input$gobutton, ignoreInit = TRUE)

  })
}




ui <- fluidPage(salesUI("sales1"))

server <- function(input, output, session) {
  salesServer("sales1", df = reactive({ sales }) )
}

shinyApp(ui, server)
