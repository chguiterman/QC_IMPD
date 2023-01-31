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
      tableOutput("contents"),
      # Download RMD
      downloadButton("report", "Generate report")
     )
    
  )
)


# Server ------------------------------------------------------------------

server <- function(input, output) {

  meta_upload <- eventReactive(input$meta, {
    meta_in <- input$meta
    meta_ext <- file_ext(meta_in$name)
    if (meta_ext == "csv") {
      read_csv(meta_in$datapath)
    } else  read_excel(meta_in$datapath)
  }
  )
  
  fhx_upload <- eventReactive(input$fhx, {
    data.frame(file_name = input$fhx$name) %>% 
      mutate(FHX = map(input$fhx$datapath, ~read_fhx(.x, verbose=FALSE)),
             n_trees = map_dbl(FHX, ~length(series_names(.x))))
    
 })                           

  comb_dat <- reactive(
    inner_join(meta_upload(), fhx_upload(), 
               by = c("FHX_FILENAME" = "file_name")
    )
  )

output$contents <- renderTable({
  # meta_upload()[, c("NAME_OF_SITE", "SITE_CODE")]
  # fhx_upload() %>% 
  #   select(file_name, n_trees)
  comb_dat() %>% 
    select(SITE_CODE, n_trees)
  })

# https://bookdown.org/yihui/rmarkdown-cookbook/child-document.html
# https://shiny.rstudio.com/articles/generating-reports.html
# https://bookdown.org/yihui/rmarkdown-cookbook/dynamic-yaml.html

output$report <- downloadHandler(
  # For PDF output, change this to "report.pdf"
  filename = "QC_report.pdf",
  content = function(file) {
    # Copy the report file to a temporary directory before processing it, in
    # case we don't have write permissions to the current working dir (which
    # can happen when deployed).
    tempReport <- file.path(tempdir(), "QC_report.Rmd")
    file.copy("QC_report.Rmd", tempReport, overwrite = TRUE)
    
    # Set up parameters to pass to Rmd document
    params <- list(NAME_OF_SITE = comb_dat()$NAME_OF_SITE[[1]],
                   FHX = comb_dat()$FHX[[1]],
                   SITE_CODE = comb_dat()$SITE_CODE[[1]],
                   CONTRIBUTORS = comb_dat()$CONTRIBUTORS[[1]],
                   SPECIES_CODE = comb_dat()$SPECIES_CODE[[1]]
                   )
    
    # Knit the document, passing in the `params` list, and eval it in a
    # child of the global environment (this isolates the code in the document
    # from the code in this app).
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
    # for (i in nrow(comb_dat())) {
    #   rmarkdown::render(tempReport, output_file = file,
    #                     params = params,
    #                     envir = new.env(parent = globalenv())
    # }
  }
)
}

# Run the application 
shinyApp(ui = ui, server = server)
