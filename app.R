#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(burnr)
library(dplyr)
library(purrr)
library(tidyr)
library(readr)
library(readxl)
library(tools)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Quality Control for Tree-Ring Fire History Submissions to the IMPD"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select metadata entry
      h5("Upload an IMPD submission workbook"),
      fileInput(inputId = "meta",
                label = NULL,
                multiple = FALSE,
                accept = c(".xlsx",
                           ".xls",
                           ".csv")),

      # Input: Select fhx file(s) ----
      h5("Upload FHX File(s)"),
      fileInput(inputId = "fhx", 
                label = NULL,
                multiple = TRUE,
                accept = c(".FHX",
                           ".fhx"))
    ),

    # Main panel for displaying outputs ----
    mainPanel(
      includeMarkdown("text_intro.Rmd"),
      # Output: Data file ----
      tableOutput("contents")

     )
    
  )
)


# Server ------------------------------------------------------------------

server <- function(input, output) {

  meta_upload <- eventReactive(input$meta, {
    meta_in <- input$meta
    meta_ext <- file_ext(meta_in$name)
    if (meta_ext == ".csv") {
      read_csv(meta_in$datapath)
    } else  read_excel(meta_in$datapath)
  }
  )
                           


output$contents <- renderTable({
  meta_upload()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
