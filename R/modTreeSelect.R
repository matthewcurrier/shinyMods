# 100 row dataset of cities
# Coloumn for US State, County, and City
# Column for population
# Column flag for whether city has professional sports team
#

library(nflreadr)
nfl_2024 <- load_rosters()


nfl_positions <- nfl_2024 |>
  select(team, position) |>
  create_tree()

ui <- fluidPage(
  titlePanel("NFL Roster Explorer"),

  sidebarLayout(
    sidebarPanel(
      treeInput(
        inputId = "ID2",
        label = "Select NFL STuff:",
        choices = nfl_positions,,
        returnValue = "text",
        closeDepth = 0
      ),
      downloadButton("downloadData", "Download Selected Data")
    ),

    mainPanel(
      DTOutput("cityTable")
    )
  )
)

server <- function(input, output, session) {

  # filtered_data <- reactive({
  #   if(is.null(input$ID2)) {
  #     nfl_2024
  #   } else {
  #     nfl_2024[nfl_2024$city %in% input$ID2,]
  #   }
  # })

  output$cityTable <- renderDT({
    datatable(filtered_data(),
              options = list(pageLength = 10,
                             scrollX = TRUE))
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("nfl_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
