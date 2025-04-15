library(shiny)
ui <- fluidPage(
  titlePanel("Hello World!"),
  fileInput("file", "Upload CSV"),
  tableOutput("tbl")
)
server <- function(input, output) {
  output$tbl <- renderTable({
    req(input$file)
    read.csv(input$file$datapath)
  })
}
shinyApp(ui, server)
