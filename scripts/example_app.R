library(bslib)
library(shiny)
library(shinyMods)
ui <- fluidPage(salesUI("sales1"))
server <- function(input, output, session) {
  salesServer("sales1",
              df = reactive({ sales }),
              dropdowns = list(
                level_1 = "TERRITORY",
                level_2 = "CUSTOMERNAME",
                level_3 = "ORDERNUMBER")
              ,
              cols_for_table = c("TERRITORY", "CUSTOMERNAME", "ORDERNUMBER", "SALES")
  )
}
shinyApp(ui, server)
