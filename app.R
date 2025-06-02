library(shiny)
library(bslib)
library(dplyr)

# Define UI for slider demo app ----
ui <- page_sidebar(
  
  # App title ----
  title = "Uploading Files",
  
  # Sidebar panel for inputs ----
  sidebar = sidebar(
    
    # Input: Select a file ----
    fileInput(
      "file1",
      "Choose CSV File",
      multiple = TRUE,
      accept = c(
        "text/csv",
        "text/comma-separated-values,text/plain",
        ".csv"
      )
    ),
    
    # Horizontal line ----
    # tags$hr(),
    
    # Input: Checkbox if file has header ----
    # checkboxInput("header", "Header", TRUE),
    
    # Input: Select separator ----
    # radioButtons(
    #   "sep",
    #   "Separator",
    #   choices = c(
    #     Comma = ",",
    #     Semicolon = ";",
    #     Tab = "\t"
    #   ),
    #   selected = ","
    # ),
    
    # Input: Select quotes ----
    # radioButtons(
    #   "quote",
    #   "Quote",
    #   choices = c(
    #     None = "",
    #     "Double Quote" = '"',
    #     "Single Quote" = "'"
    #   ),
    #   selected = '"'
    # ),
    
    # Horizontal line ----
    tags$hr(),
    
    # Input: Select number of rows to display ----
    radioButtons(
      "disp",
      "Display",
      choices = c(
        Head = "head",
        All = "all"
      ),
      selected = "head"
    ), 
    textInput(inputId = "sp1", label = "Species 1", 
              value = "Lemna aequinoctialis"),
    textInput(inputId = "sp2", label = "Species 2", 
              value = "Salvinia minima"),
    sliderInput(inputId = "pH", "pH of water", 
                min = 1, max = 13, value = 7, step = 0.1), 
    sliderInput( inputId = "light", label = "Light availability", 
                 min = 0, max = 100, value = c(35, 65)),
    textInput(inputId = "school", label = "School Name"),
    textInput(inputId = "teacher", label = "Teacher Name"),
    textAreaInput(inputId = "notes", 
                  label =  "Notes on experiment", 
                  value = "Did the experiment go according to plans?"
    ), 
    uiOutput("downloadData")
  ),
  
  # Output: Data file ----
  tableOutput("contents")
)

# Define server logic to read selected file ----
server <- function(input, output) {
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    df_in <- read.csv(
      file = input$file1$datapath)
    df <- format_data(df_in)
    
    if (input$disp == "head") {
      return(head(df))
    } else {
      return(df)
    }
    
  })
  
  
  output$downloadData <- renderUI({
    # checkboxInput("header", "Header", TRUE)
    req(input$file1)
    downloadButton("dl", "Download Formatted Data")
  })
  
  
  output$dl <- downloadHandler(
    filename = "formatted_data.csv", 
    # function() {"cleaned_data.csv"},
    content  = function(file) {
      
      in_file <- input$file1
      req(in_file)
      
      ext <- tools::file_ext(in_file$datapath)
      validate(need(ext == "csv", "Please upload a csv file"))
      
      raw_dat <- read.csv(in_file$datapath)
      
      dat_out <- format_data(raw_dat = raw_dat)
      write.csv(x = dat_out, file = file, row.names = F)
    }
  )
  
  format_data <- function(raw_dat){
    
    # Treatments:
    # t1 = Lemna common, salvinia rare
    # t2 = Equal
    # t3 = Lemna rare, salvinia common
    
    trts <- c("t1", "t2", "t3")
    spp <- c("sp1", "sp2")
    trials <- c("1", "2")
    
    colset <- expand.grid(val = c("scale", spp), 
                          trt = trts, stage = trials)
    
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
    
    new_cols <- 
      c("Name", "LabSection", "Treatment", 
        "Lemna_initial", "Salvinia_initial", 
        "Lemna_final", "Salvinia_final")
    
    names(t3) <- names(t2) <- names(t1) <- new_cols
    
    dat <- rbind(t1, t2, t3) |> 
      select(1:3, 6, 4, 7, 5) |> arrange(Name)
    
    # Looks OK
    return(dat)
  }
  
  
}

# Create Shiny app ----
shinyApp(ui, server)