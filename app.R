library(shiny)
options(shiny.trace = TRUE)

format_data <- function(raw_dat){
  trts <- c("t1", "t2", "t3")
  spp <- c("sp1", "sp2")
  trials <- c("1", "2")
  
  colset <- expand.grid(val = c("scale", spp), trt = trts, stage = trials)
  cols <- apply(colset, MARGIN = 1, FUN = paste, collapse = ".")
  names(raw_dat) <- c("first", "last", "section", cols)
  
  dat_density <- raw_dat |>
    mutate(name = paste(last, first, sep = ", ")) |>
    select(!contains("scale"), -first, -last)
  
  t1 <-  dat_density |>
    mutate(trt = "Lemna_common:Salvinia_rare") |>
    select(name, section, trt, contains("t1"))
  t2 <-  dat_density |>
    mutate(trt = "Lemna50:Salvinia50") |>
    select(name, section, trt, contains("t2"))
  t3 <-  dat_density |>
    mutate(trt = "Lemna_rare:Salvinia_common") |>
    select(name, section, trt, contains("t3"))
  
  new_cols <- c("Name", "LabSection", "Treatment",
                "Lemna_initial", "Salvinia_initial",
                "Lemna_final", "Salvinia_final")
  
  names(t3) <- names(t2) <- names(t1) <- new_cols
  dat <- rbind(t1, t2, t3) |>
    select(1:3, 6, 4, 7, 5) |>
    arrange(Name)
  
  return(dat)
}

ui <- fluidPage(
  titlePanel("Hello World!"),
  fileInput("file", "Upload CSV"),
  tableOutput("tbl")
)
server <- function(input, output) {
  output$tbl <- renderTable({
    req(input$file)
    csv_in <- read.csv(input$file$datapath)
    format_data(csv_in)
  })
  
  
}
shinyApp(ui, server)
