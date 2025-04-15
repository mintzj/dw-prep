
library(shiny)
library(bslib)
library(reshape2)
library(stringr)
library(dplyr)
library(readr)
library(DT)

# ---- Global Setup ----
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

# ---- UI ----
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  titlePanel("Measuring Interaction Strength: Data Preparation"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File", accept = ".csv"),
      helpText("Once your data is loaded, a formatted table will appear on the right.")
    ),
    mainPanel(
      DTOutput("data_table"),
      verbatimTextOutput("file1_contents"),
      downloadButton("downloadData", "Download Formatted Data")
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  raw_data <- reactive({
    req(input$file1)
    read_csv(input$file1$datapath)
  })

  formatted_data <- reactive({
    req(raw_data())
    format_data(raw_data())
  })

  output$file1_contents <- renderPrint({
    str(raw_data())
  })

  output$data_table <- renderDT({
    formatted_data()
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("formatted_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write_csv(formatted_data(), file)
    }
  )
}

shinyApp(ui, server)
