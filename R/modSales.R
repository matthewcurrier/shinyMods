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
#'
#' @examples
#' salesServer("sales1")

salesServer <- function(id, df) {
  moduleServer(id, function(input, output, session) {

    rv_reset <- reactiveVal(NULL)
    rv_go <- reactiveVal(NULL)
    # Reset functionality
    observeEvent(input$reset, {
      # Reset territory to initial state
      updateSelectInput(session, inputId = "terr", choices = sort(unique(df()$TERRITORY)))

      # Clear other dropdowns
      updateSelectInput(session, inputId = "cn", choices = NULL)
      updateSelectInput(session, inputId = "on", choices = NULL)

      output$data <- renderTable(NULL)

      rv_go(NULL)
      rv_reset(NULL)
    })


    observeEvent(df(), {
      choices <- sort(unique(df()$TERRITORY))
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




    observeEvent(input$gobutton, {

      rv_go(1)
      rv_reset(NULL)

    })

    observeEvent(rv_go(), {
      req(rv_go())
      output$data <- renderTable({
        customer() |>
          filter(ORDERNUMBER == input$on) %>%
          select(QUANTITYORDERED, PRICEEACH, PRODUCTCODE)
      })

      rv_go(NULL)
      rv_reset(NULL)
    })


  })
}




ui <- fluidPage(salesUI("sales1"))

server <- function(input, output, session) {
  salesServer("sales1", df = reactive({ sales }) )
}

shinyApp(ui, server)
