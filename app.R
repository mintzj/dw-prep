
library(shiny)
library(bslib)
library(reshape2)
library(stringr)
library(dplyr)
# library(readr)
library(ggplot2)
library(DT)

# ---- Global Setup ----
treats <- c('#15983DFF', '#FEC10BFF', '#149BEDFF')
species <- c('#EE0011FF', '#0C5BB0FF')

format_init_final <- function(duck_in){
  duckdata <- duck_in %>%
    mutate(
      TotalArea_initial = Lemna_initial + Salvinia_initial,
      TotalArea_final = Lemna_final + Salvinia_final,
      RatioInit = Lemna_initial / Salvinia_initial,
      RatioFin = Lemna_final / Salvinia_final,
      Percent_IL = Lemna_initial / TotalArea_initial,
      Percent_FL = Lemna_final / TotalArea_final,
      PCG_Lemna = (Lemna_final - Lemna_initial) / Lemna_initial,
      PCG_Salvinia = (Salvinia_final - Salvinia_initial) / Salvinia_initial,
      PCG_Difference = PCG_Lemna - PCG_Salvinia
    )

  duckdata$Treatment <- factor(
    x = duckdata$Treatment,
    levels = rev(unique(duckdata$Treatment)),
    labels = c("Sp1:low", "50:50", "Sp1:high")
  )

  duckdata$LabSection <- factor(
    x = duckdata$LabSection,
    levels = c("Monday", "Tuesday", "Wednesday", "Thursday")
  )

  return(duckdata)
}

# ---- UI ----
ui <- fluidPage(
  titlePanel("TSIIM: Duckweed Analysis - Yield & Area"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File", accept = ".csv")
    ),
    mainPanel(
      verbatimTextOutput("file1_contents"),
      h3("Figure 1"),
      plotOutput("fig1"),
      h3("Table: Summary Stats"),
      DTOutput("table1"),
      h3("Figure 2"),
      verbatimTextOutput("tab2"),
      plotOutput("fig2"),
      h3("Figure 3"),
      p("Placeholder for future content")
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  raw_data <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath)
  })

  duck_data <- reactive({
    req(raw_data())
    format_init_final(raw_data())
  })

  output$file1_contents <- renderPrint({
    str(raw_data())
  })

  output$fig1 <- renderPlot({
    ggplot(duck_data(), aes(x = Treatment, y = TotalArea_final, fill = Treatment)) +
      geom_boxplot() +
      scale_fill_manual(values = treats) +
      labs(title = "Final Yield by Treatment", y = "Total Area", x = "") +
      theme_minimal()
  })

  output$table1 <- renderDT({
    datatable(duck_data())
  })

  output$tab2 <- renderPrint({
    summary(duck_data()[, c("PCG_Lemna", "PCG_Salvinia", "PCG_Difference")])
  })

  output$fig2 <- renderPlot({
    ggplot(duck_data(), aes(x = Treatment, y = PCG_Difference, fill = Treatment)) +
      geom_boxplot() +
      scale_fill_manual(values = treats) +
      labs(title = "Per Capita Growth Difference", y = "PCG Difference", x = "") +
      theme_minimal()
  })
}

shinyApp(ui, server)
